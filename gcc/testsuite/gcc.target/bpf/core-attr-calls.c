/* Test for BPF CO-RE __attribute__((preserve_access_index)) with accesses on
   LHS and both LHS and RHS of assignment with calls involved.  */

/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -mco-re -masm=normal" } */

struct U {
  int c;
  struct V {
    int d;
    int e[4];
    int f;
    int *g;
  } v;
};

struct T {
  int a;
  int b;
  struct U u;
  struct U *ptr_u;
  struct U *array_u;
} __attribute__((preserve_access_index));

extern struct U *get_other_u(struct U *);
extern struct V *get_other_v(struct V *);

void
func (struct T *t, int i)
{
  /* Since we are using the builtin all accesses are converted to CO-RE.  */
  /* 0:3    0:0   */
  __builtin_preserve_access_index(({ get_other_u(t->ptr_u)->c = 42; }));

  /* This should not pass-through CO-RE accesses beyond the call since struct U
     is not explicitly marked with preserve_access_index. */
  /* 0:3  */
  get_other_u(t->ptr_u)->c = 43;

  /* 0:2:1  */
  get_other_v(&t->u.v)->d = 44;
}

/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:3\"\\)" 2 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:0\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:2:1\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct T\\)" 3 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct U\\)" 1 } } */

