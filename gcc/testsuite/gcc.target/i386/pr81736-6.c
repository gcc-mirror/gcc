/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

struct foo
{
  int head;
} a;

int
bar (void)
{
  return a.head != 0;
}

/* No need to use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
