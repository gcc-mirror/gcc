/* Test scalar_storage_order attribute and pointer fields */

/* { dg-do run } */
/* { dg-options "-Wno-pedantic" } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
struct __attribute__((scalar_storage_order("big-endian"))) Rec
{
  int *p;
};
#else
struct __attribute__((scalar_storage_order("little-endian"))) Rec
{
  int *p;
};
#endif

int main (int argc)
{
  struct Rec r = { &argc };
  int *p = &argc;

  if (__builtin_memcmp (&r.p, &p, sizeof (int *)) != 0)
    __builtin_abort ();

  return 0;
}
