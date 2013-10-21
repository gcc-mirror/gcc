/* { dg-do run } */
/* { dg-options "-mno-sse2 -mno-popcnt -mno-avx" } */


__attribute__((always_inline,target("avx2")))
__inline int c1 ()
{
  return 0;
}

__attribute__((always_inline,target("avx")))
__inline int c2 ()
{
  return 0;
}

__attribute__((always_inline,target("popcnt")))
__inline int c3 ()
{
  return 0;
}

__attribute__((always_inline,target("sse4.2")))
__inline int c4 ()
{
  return 0;
}

__attribute__((always_inline,target("sse4.1")))
__inline int c5 ()
{
  return 0;
}

__attribute__((always_inline,target("ssse3")))
__inline int c6 ()
{
  return 0;
}

__attribute__((always_inline,target("sse3")))
__inline int c7 ()
{
  return 0;
}

__attribute__((always_inline,target("sse2")))
__inline int c8 ()
{
  return 0;
}

int nop ()
{
  return 1;
}

#pragma GCC push_options
#pragma GCC target("sse2")
int C8 ()
{
  return c8 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("sse3")
int C7 ()
{
  return c7 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("ssse3")
int C6 ()
{
  return c6 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("sse4.1")
int C5 ()
{
  return c5 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("sse4.2")
int C4 ()
{
  return c4 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("popcnt")
int C3 ()
{
  return c3 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("avx")
int C2 ()
{
  return c2 ();
}
#pragma GCC pop_options


#pragma GCC push_options
#pragma GCC target("avx2")
int C1 ()
{
  return c1 ();
}
#pragma GCC pop_options

int main ()
{
  return C1 () + C2 () + C3 () + C4 () + C5 () + C6 () + C7 () + C8 ();
}
