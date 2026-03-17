/* Test for BPF CO-RE __attribute__((preserve_access_index)) with accesses on
   LHS and both LHS and RHS of assignment.  */

/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -mco-re -masm=normal" } */

struct other
{
  char c;
  int i;
};

struct inner_attr1
{
  int i1;
  int i2;
} __attribute__((preserve_access_index));

struct inner_noattr
{
  int i1;
  int i2;
};

union A_noattr
{
  struct inner_attr1 inner_attr;
  struct inner_noattr inner_no_attr;
  struct inner_noattr *inner_p;
};
union A_attr
{
  struct inner_attr1 inner_attr;
  struct inner_noattr inner_no_attr;
  struct inner_noattr *inner_p;
} __attribute__((preserve_access_index));


struct outer_noattr
{
  struct other *other;
  struct inner_attr
  {
    int i1;
    int i2;
  } __attribute__((preserve_access_index)) inner;
  struct inner_noattr inner_no_attr;
  struct inner_attr1 *inner_p;
  union A_attr a_attr;
  union A_noattr a_noattr;
};

struct outer_attr
{
  struct other *other;
  struct inner_attr
  {
    int i1;
    int i2;
  } __attribute__((preserve_access_index)) inner;

  struct inner_noattr inner_no_attr;
  struct inner_attr1 *inner_p;
  union A_attr a_attr;
  union A_noattr a_noattr;
} __attribute__((preserve_access_index));

extern void use_value(int);

void
func (struct outer_noattr *o, struct outer_attr *o_attr)
{
  /* 0:1 */
  o->inner.i2 = 7;
  /* 0:1 */
  o->inner_p->i2 = 8;
  /* nothing */
  o->inner_no_attr.i2 = 9;

  /* 0:2 */
  o_attr->inner_no_attr.i2 = 10;

  /* nothing */
  o->a_noattr.inner_no_attr.i1 = 1;
  /* 0:1 */
  o->a_attr.inner_no_attr.i1 = 2;
  /* 0:0 */
  o->a_noattr.inner_attr.i1 = 3;
  /* 0:4:0:0 */
  o_attr->a_attr.inner_attr.i1 = 4;
  /* 0:5	0:0 */
  o_attr->a_noattr.inner_attr.i1 = 5;

  /* This would still force everything as being attributed, although none of
     the structs has the attribute.  */
  /* 0:5:2	0:0 */
  __builtin_preserve_access_index (({ o->a_noattr.inner_p->i1 = 20; }));

  /* 0:2 */
  struct inner_noattr d = o_attr->inner_no_attr;
  use_value(d.i2);
}


extern void use(void *);

struct udphdr {
	int source;
	int dest;
	int len;
	int check;
} __attribute__((preserve_access_index));

union l4hdr {
	struct udphdr udp;
	int c;
};

struct v4hdr {
	int a;
	union l4hdr l4hdr;
	int b;
};

void gimple_loop_error(void)
{
  struct v4hdr h_outer;
  h_outer.l4hdr.udp.source = 1024;
  use(&h_outer.l4hdr.udp.source);
}

/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:5.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:4:0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:5:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:0\"\\)" 4 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:1\"\\)" 3 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:2\"\\)" 2 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:5\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:4:0:0\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:5:2\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct inner_attr\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct inner_attr1\\)" 3 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct outer_attr\\)" 4 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(union A_attr\\)" 1 } } */


