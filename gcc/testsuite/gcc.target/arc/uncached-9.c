/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdlib.h>

struct uncached_st
{
  int value;
} __attribute__((uncached));

struct cached_st
{
  int value;
};

struct uncached_st g_uncached_st =
  {
    .value = 17
  };

struct cached_st g_cached_st =
  {
    .value = 4
  };

void __attribute__((noinline)) test_struct_copy (void)
{
  g_cached_st.value = g_uncached_st.value;
}

int main (void)
{
  test_struct_copy();

  if (g_cached_st.value != g_uncached_st.value)
    abort ();

  return 0;
}
