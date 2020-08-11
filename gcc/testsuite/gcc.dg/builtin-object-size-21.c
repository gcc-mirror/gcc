/* PR middle-end/92815 - spurious -Wstringop-overflow writing into
   a flexible array of an extern struct
   { dg-do compile }
   { dg-options "-Wall -fdump-tree-optimized" }
   { dg-require-effective-target large_initializer } */

#define PTRDIFF_MAX __PTRDIFF_MAX__

typedef __SIZE_TYPE__ size_t;

#define bos0(expr) __builtin_object_size (expr, 0)
#define bos1(expr) __builtin_object_size (expr, 1)
#define bos2(expr) __builtin_object_size (expr, 2)
#define bos3(expr) __builtin_object_size (expr, 3)

void fail (const char*, ...);

#define A(x, n01, n23)							\
  ((bos0 (&x) == n01 ? (void)0 : fail (#x, __LINE__, bos0 (&x), n01)),	\
   (bos1 (&x) == n01 ? (void)0 : fail (#x, __LINE__, bos1 (&x), n01)),	\
   (bos2 (&x) == n23 ? (void)0 : fail (#x, __LINE__, bos2 (&x), n23)),	\
   (bos3 (&x) == n23 ? (void)0 : fail (#x, __LINE__, bos3 (&x), n23)))

struct Ax_m3 { char a[PTRDIFF_MAX - 3], ax[]; };

struct Ax_m3 xm3_0 = { { 0 } };
struct Ax_m3 xm3_1 = { { 0 }, { 1 } };
struct Ax_m3 xm3_2 = { { 0 }, { 1, 2 } };
struct Ax_m3 xm3_3 = { { 0 }, { 1, 2, 3 } };
struct Ax_m3 xm3_4 = { { 0 }, { 1, 2, 3, 3 } };   // { dg-error "too large" }

void test_axm3 (void)
{
  A (xm3_0, sizeof xm3_0, sizeof xm3_0);
  A (xm3_1, sizeof xm3_1 + 1, sizeof xm3_1 + 1);
  A (xm3_2, sizeof xm3_2 + 2, sizeof xm3_2 + 2);
  A (xm3_3, (size_t)-1, 0);   // expect failure
  A (xm3_4, (size_t)-1, 0);   // expect failure
}


struct Ax_mx { char a[PTRDIFF_MAX], ax[]; };
struct Ax_mx xmx_0 = { { 0 } };
struct Ax_mx xmx_1 = { { 0 }, { 1 } };            // { dg-error "too large" }
extern struct Ax_mx xmx_x;

void test_axmx (void)
{
  A (xmx_0, (size_t)-1, 0);   // expect failure
  A (xmx_1, (size_t)-1, 0);   // expect failure
  A (xmx_x, (size_t)-1, 0);   // expect failure
}
