/* PR middle-end/84108 - incorrect -Wattributes warning for packed/aligned
   conflict on struct members
   { dg-do compile }
   { dg-options "-Wall -Wattributes" } */

#define ATTR(list) __attribute__ (list)
#define ASSERT(e) _Static_assert (e, #e)

/* GCC is inconsistent in how it treats attribute aligned between
   variable and member declarations.  Attribute aligned alone is
   sufficient to reduce a variable's alignment requirement but
   the attribute must be paired with packed to have the same
   effect on a member.  Worse, declaring a variable both aligned
   and packed emits a warning.  */

/* Avoid exercising this since emitting a warning for these given
   the requirement for members seems like a misfeature:
   int a ATTR ((packed, aligned (2)));   // -Wattributes
   int b ATTR ((aligned (2), packed));   // -Wattributes
   ASSERT (_Alignof (a) == 2);
   ASSERT (_Alignof (b) == 2);  */

int c ATTR ((aligned (2)));           // okay (reduces alignment)
ASSERT (_Alignof (c) == 2);

struct {
  int a ATTR ((packed, aligned (2)));   /* { dg-bogus "\\\[-Wattributes" } */
  int b ATTR ((aligned (2), packed));   /* { dg-bogus "\\\[-Wattributes" } */

  /* Avoid exercising this since the attribute has no effect yet
     there is no warning.
     int c ATTR ((aligned (2)));           // missing warning?  */
} s;

ASSERT (_Alignof (s.a) == 2);
ASSERT (_Alignof (s.b) == 2);

/* ASSERT (_Alignof (s.c) == 4); */
