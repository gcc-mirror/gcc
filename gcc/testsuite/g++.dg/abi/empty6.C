// { dg-options "-Wabi" }

struct A {};

struct B {
  A a; // { dg-warning "empty" "" { xfail mmix-*-* } }
  virtual void f () {}
} __attribute__((aligned(2 * sizeof (void *))));
/* The preceding attribute is necessary on targets with
   BIGGEST_ALIGNMENT <= POINTER_SIZE to trigger the warning, as
   otherwise the offset of 'a' (i.e. POINTER_SIZE) is split into a
   non-zero DECL_FIELD_OFFSET and a zero DECL_FIELD_BIT_OFFSET,
   and then there is no discrepancy between DECL_FIELD_OFFSET and
   byte_position to warn about.  */
