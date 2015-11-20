/* Test support of scalar_storage_order attribute */

/* { dg-do compile } */

int i;

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

struct Rec r = { &i };  /* { dg-error "element is not constant" } */
