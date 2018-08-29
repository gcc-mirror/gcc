/* { dg-do compile }
   { dg-options "-O2 -Wall -Wextra -Warray-bounds -Wrestrict" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void* restrict, const void* restrict, size_t);

extern void sink (void*, ...);

struct Data {
  size_t n;
  void *p;
};

void test_copy (void)
{
  struct Data d;
  sink (&d);

  char dp1[sizeof d + 1];
  char d2x[2 * sizeof d];
  char d2xp1[2 * sizeof d + 1];

  /* During development the following would incorrectly trigger:
     warning: 'memcpy' forming offset [17, 25] is out of the bounds [0, 16]
	      of object ‘d’ with type 'struct Data' [-Warray-bounds]
     that wasn't caught by the test suite.  Make sure it is.  */
  memcpy (&dp1, d.p, sizeof dp1);       /* { dg-bogus "\\\[-Warray-bounds" } */

  /* Likewise.  */
  memcpy (&d2x, d.p, sizeof d2x);       /* { dg-bogus "\\\[-Warray-bounds" } */
  memcpy (&d2xp1, d.p, sizeof d2xp1);   /* { dg-bogus "\\\[-Warray-bounds" } */

  sink (&d, &dp1, &d2x, &d2xp1);
}
