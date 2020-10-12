/* PR c++/96511 - Incorrect -Wplacement-new on POINTER_PLUS into an array
   with 4-byte elements
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __SIZE_TYPE__  size_t;

void* operator new (size_t, void *p) { return p; }

void test_a1_int16 ()
{
  int16_t a3[3];                    // { dg-message "declared here" }

  new (a3) int16_t;
  new (a3 + 1) int16_t;
  new (a3 + 2) int16_t;             // { dg-bogus "\\\[-Wplacement-new" }
  new (&a3[1]) int16_t;
  new (&a3[0] + 1) int16_t;
  new (&a3[0] + 2) int16_t;         // { dg-bogus "\\\[-Wplacement-new" }
  new (&a3[0] + 3) int16_t;         // { dg-warning "\\\[-Wplacement-new" }
}

void test_a1_int32 ()
{
  int16_t a3[3];

  new (a3 + 1) int32_t;             // { dg-bogus "\\\[-Wplacement-new" }
  new (&a3[1]) int32_t;
  new (&a3[0] + 1) int32_t;         // { dg-bogus "\\\[-Wplacement-new" }
  new (&a3[0] + 2) int32_t;         // { dg-warning "\\\[-Wplacement-new" }
}


void test_a2 ()
{
  int16_t a23[2][3];

  new (a23 + 1) int16_t;            // { dg-bogus "\\\[-Wplacement-new" }
  new (&a23[1]) int16_t;
  new (&a23[2]) int16_t;            // { dg-warning "\\\[-Wplacement-new" }

  new (&a23[0][0] + 1) int16_t;
  new (&a23[0][0] + 2) int16_t;
  // Deriving a pointer to the next array from one to an element of
  // the prior array isn't valid even if the resulting pointer points
  // to an element of the larger array.  Verify it's diagnosed.
  new (&a23[0][0] + 3) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][0] + 4) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][0] + 5) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][0] + 6) int16_t;     // { dg-warning "\\\[-Wplacement-new" }

  new (&a23[0][1] + 1) int16_t;
  new (&a23[0][1] + 2) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][1] + 3) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][1] + 4) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][1] + 5) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][1] + 6) int16_t;     // { dg-warning "\\\[-Wplacement-new" }

  new (&a23[0][2] + 1) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][2] + 2) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][2] + 3) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][2] + 4) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][2] + 5) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[0][2] + 6) int16_t;     // { dg-warning "\\\[-Wplacement-new" }

  new (&a23[1][0]) int16_t;
  new (&a23[1][0] + 1) int16_t;
  new (&a23[1][0] + 2) int16_t;
  new (&a23[1][0] + 3) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[1][0] + 4) int16_t;     // { dg-warning "\\\[-Wplacement-new" }

  new (&a23[1][1]) int16_t;
  new (&a23[1][2]) int16_t;
  new (&a23[1][2] + 1) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[1][3]) int16_t;         // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[1][3] + 1) int16_t;     // { dg-warning "\\\[-Wplacement-new" }

  new (&a23[2][0]) int16_t;         // { dg-warning "\\\[-Wplacement-new" }
  new (&a23[2][0] + 1) int16_t;     // { dg-warning "\\\[-Wplacement-new" }
}
