/* PR middle-end/91490 - bogus argument missing terminating nul warning
   on strlen of a flexible array member
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

#define INT_MAX       __INT_MAX__
#define PTRDIFF_MAX   __PTRDIFF_MAX__
#define SIZE_MAX      __SIZE_MAX__

struct A0 { char n, a[0]; };
struct A1 { char n, a[1]; };
struct Ax { char n, a[]; };

const struct A0 a0 = { };
const struct A0 a0_0 = { 0 };
const struct A0 a0_0_ = { 0, { } };

const struct A0 a1 = { };
const struct A0 a1_0 = { 0 };
const struct A0 a1_0_ = { 0, { } };

const struct Ax ax= { };
const struct Ax ax_0 = { 0 };
const struct Ax ax_0_ = { 0, { } };

void sink (unsigned);

#define T(x)   sink (__builtin_strlen (x))

void test_zero_length_array (void)
{
  T (a0.a);                   // { dg-warning "\\\[-Warray-bounds" }
  T (a0.a - 1);               // { dg-warning "\\\[-Warray-bounds" }
  T (a0.a + 1);               // { dg-warning "\\\[-Warray-bounds" }
  T (a0.a + 9);               // { dg-warning "\\\[-Warray-bounds" }
  T (a0.a + INT_MAX);         // { dg-warning "\\\[-Warray-bounds" }
  T (a0.a + PTRDIFF_MAX);     // { dg-warning "\\\[-Warray-bounds" }
  T (a0.a + SIZE_MAX);        // { dg-warning "\\\[-Warray-bounds" }

  T (a0_0.a);                 // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0.a - 1);             // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0.a + 1);             // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0.a + 9);             // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0.a + INT_MAX);       // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0.a + PTRDIFF_MAX);   // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0.a + SIZE_MAX);      // { dg-warning "\\\[-Warray-bounds" }

  T (a0_0_.a);                // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0_.a - 1);            // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0_.a + 1);            // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0_.a + 9);            // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0_.a + INT_MAX);      // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0_.a + PTRDIFF_MAX);  // { dg-warning "\\\[-Warray-bounds" }
  T (a0_0_.a + SIZE_MAX);     // { dg-warning "\\\[-Warray-bounds" }
}

void test_one_element_array (void)
{
  T (a1.a - 1);               // { dg-warning "\\\[-Warray-bounds" }
  T (a1.a + 1);               // { dg-warning "\\\[-Warray-bounds" }
  T (a1.a + 9);               // { dg-warning "\\\[-Warray-bounds" }
  T (a1.a + INT_MAX);         // { dg-warning "\\\[-Warray-bounds" }
  T (a1.a + PTRDIFF_MAX);     // { dg-warning "\\\[-Warray-bounds" }
  T (a1.a + SIZE_MAX);        // { dg-warning "\\\[-Warray-bounds" }

  T (a1_0.a - 1);             // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0.a + 1);             // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0.a + 9);             // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0.a + INT_MAX);       // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0.a + PTRDIFF_MAX);   // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0.a + SIZE_MAX);      // { dg-warning "\\\[-Warray-bounds" }

  T (a1_0_.a - 1);            // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0_.a + 1);            // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0_.a + 9);            // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0_.a + INT_MAX);      // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0_.a + PTRDIFF_MAX);  // { dg-warning "\\\[-Warray-bounds" }
  T (a1_0_.a + SIZE_MAX);     // { dg-warning "\\\[-Warray-bounds" }
}

void test_flexible_array_member (void)
{
  T (ax.a);                   // { dg-warning "\\\[-Warray-bounds" }
  T (ax.a - 1);               // { dg-warning "\\\[-Warray-bounds" }
  T (ax.a + 1);               // { dg-warning "\\\[-Warray-bounds" }
  T (ax.a + 9);               // { dg-warning "\\\[-Warray-bounds" }
  T (ax.a + INT_MAX);         // { dg-warning "\\\[-Warray-bounds" }
  T (ax.a + PTRDIFF_MAX);     // { dg-warning "\\\[-Warray-bounds" }
  T (ax.a + SIZE_MAX);        // { dg-warning "\\\[-Warray-bounds" }

  T (ax_0.a);                 // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0.a - 1);             // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0.a + 1);             // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0.a + 9);             // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0.a + INT_MAX);       // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0.a + PTRDIFF_MAX);   // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0.a + SIZE_MAX);      // { dg-warning "\\\[-Warray-bounds" }

  T (ax_0_.a);                // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0_.a - 1);            // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0_.a + 1);            // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0_.a + 9);            // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0_.a + INT_MAX);      // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0_.a + PTRDIFF_MAX);  // { dg-warning "\\\[-Warray-bounds" }
  T (ax_0_.a + SIZE_MAX);     // { dg-warning "\\\[-Warray-bounds" }
}

