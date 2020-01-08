/* { dg-do compile } */
/* { dg-options "" } */

/* "p" modifier can't be used to generate a valid memory address with ILP32.  */
/* { dg-skip-if "" { aarch64*-*-* && ilp32 } } */
/* { dg-skip-if "'p' is not supported for GCN" { amdgcn-*-* } } */

int main()
{
  int x, y, z;

  asm volatile ("test0 X%0Y%[arg]Z" : [arg] "=g" (x));
  asm volatile ("test1 X%[out]Y%[in]Z" : [out] "=g" (y) : [in] "0"(y));
  asm volatile ("test2 X%a0Y%a[arg]Z" : : [arg] "p" (&z));
  asm volatile ("test3 %[in]" : [inout] "=g"(x) : "[inout]" (x), [in] "g" (y));
}

/* { dg-final { scan-assembler {test0 X(.*)Y\1Z} } } */
/* { dg-final { scan-assembler {test1 X(.*)Y\1Z} } } */
/* { dg-final { scan-assembler {test2 X(.*)Y\1Z} } } */
