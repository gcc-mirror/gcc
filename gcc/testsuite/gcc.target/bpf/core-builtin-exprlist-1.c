/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

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

unsigned long
func (void)
{
  int b;
  char c;
  unsigned char uc;
  unsigned long ul;
  int ic;

  __builtin_preserve_access_index (({
    /* 1 */
    b = my_s.b;

    /* 2 */
    ic = my_s.c;

    /* 2:3 */
    uc = my_u.uc[3];

    /* 6 */
    ul = ula[6];
  }));

  return b + ic + uc + ul;
}

char
s_ptr (struct S *ps)
{
  /* 0:2 */
  char x;
  __builtin_preserve_access_index (({ x = ps->c; }));
  return x;
}

unsigned char
u_ptr (union U *pu)
{
  /* 0:2:3 */
  unsigned char x;
  __builtin_preserve_access_index (({ x = pu->uc[3]; }));
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
