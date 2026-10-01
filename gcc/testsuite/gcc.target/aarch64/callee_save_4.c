/* { dg-options "-O2 -fno-omit-frame-pointer -mtune=generic" } */

struct s { int x; unsigned char flag; };
void set_off (struct s *, int);

int
foo (struct s *p, int val)
{
  set_off (p, val);
  p->flag = val >= 0;
  return 0;
}

/* { dg-final { scan-assembler {\tstp\tx19, x20, \[sp,} } } */
/* { dg-final { scan-assembler {\tldp\tx19, x20, \[sp,} } } */
/* { dg-final { scan-assembler-not {\tstr\tw[0-9]+, \[sp,} } } */
