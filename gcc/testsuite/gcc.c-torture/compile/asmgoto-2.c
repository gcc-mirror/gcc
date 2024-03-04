/* This test should be switched off for a new target with less than 4 allocatable registers */
/* { dg-do compile { target asm_goto_with_outputs } } */
int
foo (void)
{
  int x, y, z, v;
  
  asm goto ("": "=r" (x), "=r" (y), "=r" (z), "=r" (v) : : : lab, lab2, lab3, lab4);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
 lab4:
  return v;
}

int
foo2 (void)
{
  int x = 0, y = 1, z = 2, v = 3;
  
  asm goto ("": "+r" (x), "+r" (y), "+r" (z), "+r" (v) : : : lab, lab2, lab3, lab4);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
 lab4:
  return v;
}

int
foo3 (void)
{
  int x, y, z, v;
  
  asm goto ("": "=rm" (x), "=mr" (y), "=rm" (z), "=mr" (v) : : : lab, lab2, lab3, lab4);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
 lab4:
  return v;
}

int
foo4 (void)
{
  int x, y, z, v;
  
  asm goto ("": "=r,m" (x), "=m,r" (y), "=r,m" (z), "=m,r" (v) : : : lab, lab2, lab3, lab4);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
 lab4:
  return v;
}
