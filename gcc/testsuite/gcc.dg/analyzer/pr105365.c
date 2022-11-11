/* { dg-require-effective-target int128 } */

typedef _Float32 f32;
typedef _Complex _Float32 cf32;
_Float32 g;
__int128 i;

extern void bar(int);

void
foo(_Float32 k) {
  f32 f = 0;
  f /= (_Complex char)__builtin_llround(g);
  k /= (cf32)__builtin_copysignf(0, i);
  bar(f + k);
  foo(0); /* { dg-warning "infinite recursion" } */
}
