/* { dg-do compile } */
/* { dg-options "" } */

int main()
{
  int x;

  asm volatile ("test0 X%0Y%[arg]Z" : [arg] "=g" (x));
  asm volatile ("test1 X%[out]Y%[in]Z" : [out] "=g" (x) : [in] "0"(x));
}

/* ??? Someone explain why the back reference dosn't work.  */
/* { dontdg-final { scan-assembler "test0 X(.*)Y\1Z" } } */
/* { dontdg-final { scan-assembler "test1 X(.*)Y\1Z" } } */
