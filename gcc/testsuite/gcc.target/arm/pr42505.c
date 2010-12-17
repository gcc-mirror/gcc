/* { dg-options "-mthumb -Os -march=armv5te" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler-not "str\[\\t \]*r.,\[\\t \]*.sp," } } */

struct A {
 int f1;
 int f2;
};

int func(int c);

/* This function should not need to spill anything to the stack.  */
int test(struct A* src, struct A* dst, int count)
{
  while (count--) {
    if (!func(src->f2)) {
        return 0;
      }
      *dst++ = *src++;
  }

  return 1;
}
