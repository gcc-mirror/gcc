/* PR debug/113519 */
/* Reported by Zdenek Sojka <zsojka@seznam.cz> */

/* { dg-do compile } */
/* { dg-options "-g -fdebug-types-section" } */

enum E { X };

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
struct __attribute__((scalar_storage_order("big-endian")))
{
  enum E e;
} S;
#else
struct __attribute__((scalar_storage_order("little-endian")))
{
  enum E e;
} S;
#endif
