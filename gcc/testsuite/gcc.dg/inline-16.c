/* { dg-do link } */
/* { dg-options "-std=c99" } */

static inline __SIZE_TYPE__
func1(const volatile void * base, __SIZE_TYPE__ byteOffset)
{
  volatile __SIZE_TYPE__ *addr
    = (volatile __SIZE_TYPE__ *)((__SIZE_TYPE__)base + byteOffset);
  return *addr;
}

static inline __SIZE_TYPE__
func2(__SIZE_TYPE__ data)
{
    return func1(&data, 0);
}

int main(int argc, char *argv[]) {
  __SIZE_TYPE__ b = func2(argc);

  return 0;
}
