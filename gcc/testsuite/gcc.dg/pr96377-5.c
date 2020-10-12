/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-fno-lax-vector-conversions" } */
/* { dg-message "use '-flax-vector-conversions' to permit conversions" "" { target *-*-* } 0 } */

typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));

struct s { __Int16x8_t x; __Int32x4_t y; };
union u1 { __Int16x8_t x; __Int32x4_t y; };
union u2 { __Int32x4_t s; __Int16x8_t y; };

void
foo (v4si i, v8hi h)
{
  struct s x1 = { i, i }; // { dg-error "incompatible types when initializing type '__Int16x8_t" }
  struct s x2 = { h, h }; // { dg-error "incompatible types" }
  struct s x3 = { i, h }; // { dg-error "incompatible types" }
  struct s x4 = { h, i };

  union u1 y1 = { i }; // { dg-error "incompatible types" }
  union u1 y2 = { h };
  union u2 y3 = { i };
  union u2 y4 = { h }; // { dg-error "incompatible types" }

  v4si z1[] = { i, i };
  v4si z2[] = { i, h }; // { dg-error "incompatible types" }
  v4si z3[] = { h, i }; // { dg-error "incompatible types" }
  v4si z4[] = { h, h }; // { dg-error "incompatible types" }
  v8hi z5[] = { i, i }; // { dg-error "incompatible types" }
  v8hi z6[] = { i, h }; // { dg-error "incompatible types" }
  v8hi z7[] = { h, i }; // { dg-error "incompatible types" }
  v8hi z8[] = { h, h };
}
