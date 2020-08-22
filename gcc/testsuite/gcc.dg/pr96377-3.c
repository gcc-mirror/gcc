/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-fno-lax-vector-conversions" } */
/* { dg-message "use '-flax-vector-conversions' to permit conversions" "" { target *-*-* } 0 } */

typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));

struct s { v8hi x; v4si y; };
union u1 { v8hi x; v4si y; };
union u2 { v4si s; v8hi y; };

void
foo (__Int32x4_t i, __Int16x8_t h)
{
  struct s x1 = { i, i }; // { dg-error "incompatible types when initializing type '__vector" }
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
