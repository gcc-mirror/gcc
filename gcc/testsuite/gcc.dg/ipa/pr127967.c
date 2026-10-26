/* { dg-do run } */
/* { dg-require-effective-target le } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3"  } */

struct S { unsigned p : 4, q : 8; };

__attribute__((noinline)) static int f(struct S s) { return s.q; }

int main() {
  union { unsigned char b[4]; struct S s; } u = {{16}};
  int r =  f(u.s);
  if (r != 1)
    __builtin_abort ();
  return 0;
}
