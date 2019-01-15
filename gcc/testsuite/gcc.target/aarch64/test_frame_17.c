/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Test reuse of stack adjustment temporaries.  */

void foo ();

/* Should only use 1 mov and re-use it.  */
int reuse_mov (int i)
{
  int arr[1025];
  return arr[i];
}

/* Should use 2 movs because x12 is live.  */
int no_reuse_mov_live (int i)
{
  int arr[1025];
  register long long a __asm("x12");
  a = a+1;
  return arr[i] + a;
}

/* Should use 2 movs because its not a leaf function.  */
int no_reuse_mov (int i)
{
  int arr[1025];
  foo ();
  return arr[i];
}

/* { dg-final { scan-assembler-times "mov\tx12, \[0-9\]+" 5 } } */
