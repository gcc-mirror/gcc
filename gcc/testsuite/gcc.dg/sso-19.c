/* { dg-do compile } */
/* { dg-options "-std=c23" } */
/* PR c/104822 */

#include <stddef.h>

struct Sb {
  int i;
} __attribute__((scalar_storage_order("big-endian")));
struct Sl {
  int i;
} __attribute__((scalar_storage_order("little-endian")));

/* Neither of these should warn about incompatible scalar storage order
   as NULL pointers are compatiable with both endian. */
struct Sb *pb = nullptr; /* { dg-bogus "" } */
struct Sl *pl = nullptr; /* { dg-bogus "" } */
