/* { dg-do run }
   { dg-options "-O -Wall" } */

int
main ()
{
  int __lds *testptr = (int __lds *)(__UINTPTR_TYPE__)8;
  *testptr = 4;

  int __flat *testptr_flat = testptr;
  if (*testptr_flat != 4)
    return 1;
}
