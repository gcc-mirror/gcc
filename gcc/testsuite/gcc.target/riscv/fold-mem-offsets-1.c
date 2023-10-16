/* { dg-do compile } */
/* { dg-options "-O2 -ffold-mem-offsets" } */

void sink(int arr[2]);

void
foo(int a, int b, int i)
{
  int arr[2] = {a, b};
  arr[i]++;
  sink(arr);
}

/* The should be no negative memory offsets when using -ffold-mem-offsets.  */
/* { dg-final { scan-assembler-not "lw\t.*,-.*\\(.*\\)" } } */
/* { dg-final { scan-assembler-not "sw\t.*,-.*\\(.*\\)" } } */