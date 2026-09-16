/* PR target/54412 */
/* Check that "assign_stack_temp_for_type" safely reuses an over-aligned
   temporary across control-flow paths.  The stack slot used by Vec4q (z0)
   is reused for Vec4q (z1).  Its dynamically aligned address is computed in
   the first branch and held in a pseudo-register that is undefined along the
   second path, so the address must be recomputed when the slot is reused.  */
/* { dg-do run { target { x86_64-*-mingw* && avx2 } } } */
/* { dg-options "-O0 -mavx2 -std=gnu++17 -fno-omit-frame-pointer" } */
/* { dg-require-effective-target avx2_runtime } */

#include "avx2-check.h"

typedef long long v4di __attribute__ ((vector_size (32), aligned (32)));

static inline v4di
make_v4di (long long a0, long long a1, long long a2, long long a3)
{
  return (v4di) { a0, a1, a2, a3 };
}

struct Vec256b
{
  v4di ymm;

  Vec256b () = default;
  explicit Vec256b (v4di x) : ymm (x) {}

  __attribute__ ((noinline, noclone))
  operator v4di () const
  {
    return ymm;
  }
};

struct Vec4q : public Vec256b
{
  Vec4q () = default;

  __attribute__ ((noinline, noclone))
  explicit Vec4q (v4di x)
  {
    ymm = x;
  }

  __attribute__ ((noinline, noclone))
  long long
  extract (int index) const
  {
    long long elems[4] __attribute__ ((aligned (32)));
    __builtin_memcpy (elems, &ymm, sizeof (elems));
    return elems[index & 3];
  }
};

struct Vec8q
{
  Vec256b z0;
  Vec256b z1;

  Vec8q () = default;
  Vec8q (Vec256b a, Vec256b b) : z0 (a), z1 (b) {}

  __attribute__ ((noinline, noclone))
  long long
  extract (int index) const
  {
    if ((unsigned) index < 4)
      return Vec4q (z0).extract (index);

    return Vec4q (z1).extract (index - 4);
  }
};

__attribute__ ((noinline, noclone))
static Vec8q
passthru (Vec8q x)
{
  return x;
}

__attribute__ ((noinline, noclone))
static long long
call_extract (Vec8q x, int index)
{
  Vec8q y = passthru (x);

  if ((unsigned) index >= 4)
    {
      register void *p __asm__ ("rbx") = (void *) 2;
      asm volatile ("" : : "r" (p) : "memory");
    }

  return y.extract (index);
}

static void
avx2_test ()
{
  Vec8q x (Vec256b (make_v4di (101, 102, 103, 104)),
	   Vec256b (make_v4di (201, 202, 203, 204)));

  if (call_extract (x, 1) != 102)
    __builtin_abort ();

  if (call_extract (x, 5) != 202)
    __builtin_abort ();

  if (call_extract (x, 7) != 204)
    __builtin_abort ();
}
