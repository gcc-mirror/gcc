// { dg-options "-Wabi" }

struct A {};

struct B {
  A a; // { dg-warning "empty" }
  virtual void f () {}
} __attribute__((aligned(8)));
/* The preceding attribute is necessary on targets with
   BIGGEST_ALIGNMENT <= 32 to trigger the warning, as otherwise a 32 bit
   offset is split into DECL_FIELD_OFFSET 4 and DECL_FIELD_BIT_OFFSET 0,
   and then there is no discrepancy between DECL_FIELD_OFFSET and
   byte_position to warn about.  */
