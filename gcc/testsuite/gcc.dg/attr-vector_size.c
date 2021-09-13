/* PR middle-end/89797 - ICE on a vector_size (1LU << 33) int variable
   PR c/89798 - excessive vector_size silently accepted and truncated
   { dg-do compile { target int32plus } }
   { dg-options "-Wall -Wno-unused" } */

#define ASSERT(e)    _Static_assert (e, #e)
#define VEC(N)       __attribute__ ((vector_size (N)))
#define POW2(N)      (1LLU << N)
#define CAT(a, b)    a ## b
#define CONCAT(a, b) CAT (a, b)

#define DEFVEC(storage, N)				\
  typedef VEC (POW2 (N)) char CONCAT (Vec, N);		\
  storage CONCAT (Vec, N) CONCAT (v, N);		\
  ASSERT (sizeof (CONCAT (Vec, N)) == POW2 (N));	\
  ASSERT (sizeof (CONCAT (v, N)) == POW2 (N))

DEFVEC (extern, 27);
DEFVEC (extern, 28);
DEFVEC (extern, 29);
DEFVEC (extern, 30);

#if __SIZEOF_SIZE_T__ > 4

VEC (POW2 (63)) char v63;     /* { dg-error  "'vector_size' attribute argument value '9223372036854775808' exceeds 9223372036854775807" "LP64" { target lp64 } } */

#else

VEC (POW2 (31)) char v31;     /* { dg-error  "'vector_size' attribute argument value '2147483648' exceeds 2147483647" "ILP32" { target ilp32 } } */

VEC (POW2 (32)) char v32;     /* { dg-error  "'vector_size' attribute argument value '4294967296' exceeds 2147483647" "ILP32" { target ilp32 } } */

#endif

void test_local_scope (void)
{
  DEFVEC (auto, 27);
  DEFVEC (auto, 28);
  DEFVEC (auto, 29);
  DEFVEC (auto, 30);

#if __SIZEOF_SIZE_T__ > 4

  VEC (POW2 (63)) char v63;   /* { dg-error  "'vector_size' attribute argument value '9223372036854775808' exceeds 9223372036854775807" "LP64" { target lp64 } } */

#else

  VEC (POW2 (31)) char v31;   /* { dg-error  "'vector_size' attribute argument value '2147483648' exceeds 2147483647" "ILP32" { target ilp32 } } */

  VEC (POW2 (32)) char v32;   /* { dg-error  "'vector_size' attribute argument value '4294967296' exceeds 2147483647" "ILP32" { target ilp32 } } */

#endif
}
