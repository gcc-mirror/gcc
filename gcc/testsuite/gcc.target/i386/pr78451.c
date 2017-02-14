/* PR target/78451 */
/* { dg-options "-O2 -mno-avx512f" } */

#pragma GCC push_options
#pragma GCC target ("avx512bw")

static inline int __attribute__ ((__always_inline__))
bar (void)
{
  return 0;
}

#pragma GCC push_options
#pragma GCC target ("avx512vl")

int
foo (void)
{
  return bar ();
}

#pragma GCC pop_options
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("avx512vl")
#pragma GCC target ("avx512bw")

int
baz (void)
{
  return bar ();
}

#pragma GCC pop_options
