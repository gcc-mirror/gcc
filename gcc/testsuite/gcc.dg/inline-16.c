/* { dg-do link } */
/* { dg-options "-std=c99" } */

static inline int
func1(const volatile void * base, int byteOffset)
{
  volatile int *addr = (volatile int *)((int)base + byteOffset);
  return *addr;
}

static inline int
func2(int data)
{
    return func1(&data, 0);
}

int main(int argc, char *argv[]) {
  int b = func2(argc);

  return 0;
}
