/* { dg-do compile } */
/* { dg-options "-O2 -fpic -fno-inline --save-temps" } */

void abort ();
int global_a;

int
initialize (void)
{
  global_a = 0x10;
  return global_a - 1;
}

int
main (int argc, char **argv)
{
  int a = initialize ();

  if (a != global_a - 1)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "adrp\tx\[0-9\]+, _GLOBAL_OFFSET_TABLE" 2 } } */
/* { dg-final { cleanup-saved-temps } } */
