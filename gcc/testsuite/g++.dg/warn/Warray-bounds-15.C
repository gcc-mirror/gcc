/* PR middle-end/98266 - bogus array subscript is partly outside array
   bounds on virtual inheritance
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#if __cplusplus < 201103L
// This matters for the test case.
#  define noexcept   throw ()
#endif

struct A
{
  virtual ~A () noexcept;
  const char *s;
};

struct B: virtual A { };
struct C: virtual A { };      // { dg-bogus "\\\[-Warray-bounds" }

struct D: virtual B, virtual C
{
  D (const char*);
};

void sink (void*);
void sink (D);


// Verify that accesses to the table aren't diagnosed.
void test_vtbl ()
{
  sink (D (""));
}
