/* { dg-do run } */

typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
struct X { int32_t i; int32_t j; };
void foo (int64_t *z)
{
  ((struct X *)z)->i = 0x05060708;
  ((struct X *)z)->j = 0x01020304;
  *z = 0x0102030405060708;
}

int main()
{
  int64_t l = 0;
  int64_t *p;
  asm ("" : "=r" (p) : "0" (&l));
  foo (p);
  if (l != 0x0102030405060708)
    __builtin_abort ();
  return 0;
}
