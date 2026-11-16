/* PR target/128670 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#define T(N)								\
struct S##N								\
{									\
  char pad[N];								\
  struct { char x; int : 0; } a[2];					\
};									\
									\
__attribute__((noipa)) struct S##N					\
f##N (void)								\
{									\
  return (struct S##N) { .a[1].x = 42 };				\
}

T (4)
T (5)
T (6)
T (7)

int
main (void)
{
  if (f4 ().a[1].x != 42
      || f5 ().a[1].x != 42
      || f6 ().a[1].x != 42
      || f7 ().a[1].x != 42)
    __builtin_abort ();
  return 0;
}
