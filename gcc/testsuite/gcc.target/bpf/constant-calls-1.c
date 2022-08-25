/* { dg-do compile } */

typedef void  *(*T)(void);
f1 ()
{
  ((T) 0)();
}
f2 ()
{
  ((T) 1000)();
}
f3 ()
{
  ((T) 1000000)();
}

/* { dg-final { scan-assembler "call\t0" } } */
/* { dg-final { scan-assembler "call\t1000" } } */
/* { dg-final { scan-assembler "call\t10000" } } */
