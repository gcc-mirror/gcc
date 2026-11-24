/* PR debug/128920 */

/* { dg-do run } */
/* { dg-options "-std=c23" }
/* { dg-require-effective-target float16 } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
struct __attribute__((scalar_storage_order("big-endian"))) S
{
  _Float16 x;
};
#else
struct __attribute__((scalar_storage_order("little-endian"))) S
{
  _Float16 x;
};
#endif

struct S s = { 1.5f16 };

int main(void)
{
  if (s.x != 1.5f16)
    __builtin_abort ();
  return 0;
}
