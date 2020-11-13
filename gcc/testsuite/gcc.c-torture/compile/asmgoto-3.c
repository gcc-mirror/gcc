/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

int
foo (void)
{
  int x;
  
  asm goto ("": "=a" (x) : : : lab);
 lab:
  return x;
}

int
foo2 (void)
{
  int x, y;
  
  asm goto ("": "=a" (x), "=d" (y) : : : lab, lab2);
 lab:
  return x;
 lab2:
  return y;
}

int
foo3 (void)
{
  int x, y, z;
  
  asm goto ("": "=a" (x), "=d" (y), "=c" (z) : : : lab, lab2, lab3);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
}

int
foo4 (void)
{
  int x, y, z, v;
  
  asm goto ("": "=a" (x), "=d" (y), "=c" (z) , "=b" (v) : : : lab, lab2, lab3, lab4);
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
foo5 (void)
{
  int x, y, z, v, w;
  
  asm goto ("": "=a" (x), "=d" (y), "=c" (z), "=b" (v), "=S" (w) : : : lab, lab2, lab3, lab4, lab5);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
 lab4:
  return v;
 lab5:
  return w;
}

int
foo6 (void)
{
  int x = 0, y = 1, z = 2, v = 3, w = 4;
  
  asm goto ("": "+a" (x), "+d" (y), "+c" (z), "+b" (v), "+S" (w) : : : lab, lab2, lab3, lab4, lab5);
 lab:
  return x;
 lab2:
  return y;
 lab3:
  return z;
 lab4:
  return v;
 lab5:
  return w;
}
