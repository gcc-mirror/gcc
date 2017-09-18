/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-overflow -fdump-tree-tailc-details" } */

void bar();
void foo(char *dst)
{
  char *const end = dst;
  do {
    bar();
    dst += 2;
  } while (dst < end);
}

/* The loop only iterates once because pointer overflow always has undefined
   semantics.  As a result, call to bar becomes tail call.  */
/* { dg-final { scan-tree-dump-times "Found tail call " 1 "tailc" } } */
