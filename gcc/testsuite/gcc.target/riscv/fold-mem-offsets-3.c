/* { dg-do compile } */
/* { dg-options "-O2 -ffold-mem-offsets" } */

void load(int arr[2]);

int
foo(long unsigned int i)
{
  int arr[2];
  load(arr);

  return arr[3 * i + 77];
}

/* The should be no negative memory offsets when using -ffold-mem-offsets.  */
/* { dg-final { scan-assembler-not "lw\t.*,-.*\\(.*\\)" } } */
/* { dg-final { scan-assembler-not "addi\t.*,.*,77" } } */