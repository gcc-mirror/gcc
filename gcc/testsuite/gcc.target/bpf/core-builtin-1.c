/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  int a;
  int b;
  char c;
};

union U {
  unsigned int u;
  int i;
  unsigned char uc[4];
  signed char ic[4];
};

struct S my_s;
union U my_u;

unsigned long ula[8];

#define _(x) (__builtin_preserve_access_index (x))

void
func (void)
{
  /* 1 */
  int b = _(my_s.b);

  /* 2 */
  char c = _(my_s.c);

  /* 2:3 */
  unsigned char uc = _(my_u.uc[3]);

  /* 6 */
  unsigned long ul = _(ula[6]);
}

char
s_ptr (struct S *ps)
{
  /* 0:2 */
  char x = _(ps->c);
  return x;
}

unsigned char
u_ptr (union U *pu)
{
  /* 0:2:3 */
  unsigned char x = _(pu->uc[3]);
  return x;
}

/* { dg-final { scan-assembler-times "ascii \"1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"2:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"6.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "bpfcr_type" 6 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x6c\[\t \]+\[^\n\]*core_relo_len" 1 } } */
