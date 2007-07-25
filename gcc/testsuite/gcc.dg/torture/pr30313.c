/* { dg-do compile } */

static inline void bar(){}

struct S
{
#if __INT_MAX__ == 32767
  signed int i: 16;
#elif __INT_MAX__ == 2147483647
  signed int i: 32;
#elif __INT_MAX__ == 9223372036854775807
  signed int i: 64;
#else
#error Please add support for your target here
#endif
};

int main()
{
  struct S x = {32};
  sizeof(x.i+0);
  return 0;
}
