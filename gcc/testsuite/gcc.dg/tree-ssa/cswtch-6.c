/* PR tree-optimization/120451 */
/* { dg-do compile { target elf } } */
/* { dg-options "-O2" } */

void foo (int, int);

__attribute__((noinline, noclone)) void
f1 (int v, int w)
{
  int i, j;
  if (w)
    {
      i = 129;
      j = i - 1;
      goto lab;
    }
  switch (v)
    {
    case 170:
      j = 7;
      i = 27;
      break;
    case 171:
      i = 8;
      j = 122;
      break;
    case 172:
      i = 21;
      j = -19;
      break;
    case 173:
      i = 18;
      j = 17;
      break;
    default:
      __builtin_abort ();
    }

 lab:
  foo (i, j);
}

/* { dg-final { scan-assembler ".rodata.cst16" { xfail { sparc*-*-solaris2* && { ! gas } } } } } */
