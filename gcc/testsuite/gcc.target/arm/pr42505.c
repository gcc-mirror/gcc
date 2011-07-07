/* { dg-options "-Os" }  */
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
