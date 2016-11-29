/* { dg-do compile } */
/* { dg-options "-O2 --save-temps" } */

/* Test reuse of stack adjustment temporaries.  */

void foo ();

int reuse_mov (int i)
{
  int arr[1025];
  return arr[i];
}

int no_reuse_mov (int i)
{
  int arr[1025];
  foo ();
  return arr[i];
}

/* { dg-final { scan-assembler-times "mov\tx16, \[0-9\]+" 3 } } */
