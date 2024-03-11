/* PR debug/113777 */
/* Reported by Zdenek Sojka <zsojka@seznam.cz> */

/* { dg-do compile } */
/* { dg-options "-g" } */

typedef short __attribute__((__hardbool__)) hbool;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
struct __attribute__((scalar_storage_order("big-endian")))
{
  hbool a[2];
} S;
#else
struct __attribute__((scalar_storage_order("little-endian")))
{
  hbool a[2];
} S;
#endif
