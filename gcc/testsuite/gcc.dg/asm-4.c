/* { dg-do compile } */
/* { dg-options "" } */

int main()
{
  int x, y, z;

  asm volatile ("test0 X%0Y%[arg]Z" : [arg] "=g" (x));
  asm volatile ("test1 X%[out]Y%[in]Z" : [out] "=g" (y) : [in] "0"(y));
  asm volatile ("test2 X%a0Y%a[arg]Z" : : [arg] "p" (&z));
  asm volatile ("test3 %[in]" : [inout] "=g"(x) : "[inout]" (x), [in] "g" (y));
}

/* ??? Someone explain why the back reference dosn't work.  */
/* { dontdg-final { scan-assembler "test0 X(.*)Y\1Z" } } */
/* { dontdg-final { scan-assembler "test1 X(.*)Y\1Z" } } */
/* { dontdg-final { scan-assembler "test2 X(.*)Y\1Z" } } */
