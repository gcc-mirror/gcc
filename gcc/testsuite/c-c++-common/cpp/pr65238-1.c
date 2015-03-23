/* PR preprocessor/65238 */
/* { dg-do run } */

#define A unused
#define B A
#define C B
#define D __has_attribute(unused)
#define E __has_attribute(C)
#define F(X) __has_attribute(X)
#if !__has_attribute(unused)
#error unused attribute not supported - 1
#endif
#if !__has_attribute(C)
#error unused attribute not supported - 2
#endif
#if !D
#error unused attribute not supported - 3
#endif
#if !E
#error unused attribute not supported - 4
#endif
#if !F(unused)
#error unused attribute not supported - 5
#endif
#if !F(C)
#error unused attribute not supported - 6
#endif
int a = __has_attribute (unused) + __has_attribute (C) + D + E + F (unused) + F (C);
int b = __has_attribute (unused);
int c = __has_attribute (C);
int d = D;
int e = E;
int f = F (unused);
int g = F (C);
int h = __has_attribute (
  unused
) + __has_attribute  (

C) + F (
unused

) + F
(
C
);

int
main ()
{
  if (a != 6 || b != 1 || c != 1 || d != 1 || e != 1 || f != 1 || g != 1 || h != 4)
    __builtin_abort ();
  return 0;
}
