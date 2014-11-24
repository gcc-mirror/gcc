/* PR target/63594 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-mno-mmx" { target i?86-*-* x86_64-*-* } } */
/* { dg-prune-output "non-standard ABI extension" } */

#define C1 c
#define C2 C1, C1
#define C4 C2, C2
#define C8 C4, C4
#define C16 C8, C8
#define C32 C16, C16
#define C64 C32, C32
#define C_(n) n
#define C(n) C_(C##n)

#define T(t,s) \
typedef t v##t##s __attribute__ ((__vector_size__ (s * sizeof (t))));	\
v##t##s									\
test1##t##s (t c)							\
{									\
  v##t##s v = { C(s) };							\
  return v;								\
}									\
									\
v##t##s									\
test2##t##s (t *p)							\
{									\
  t c = *p;								\
  v##t##s v = { C(s) };							\
  return v;								\
}

typedef long long llong;

T(char, 64)
T(char, 32)
T(char, 16)
T(char, 8)
T(char, 4)
T(char, 2)
T(char, 1)
T(short, 32)
T(short, 16)
T(short, 8)
T(short, 4)
T(short, 2)
T(short, 1)
T(int, 16)
T(int, 8)
T(int, 4)
T(int, 2)
T(int, 1)
T(float, 16)
T(float, 8)
T(float, 4)
T(float, 2)
T(float, 1)
T(llong, 8)
T(llong, 4)
T(llong, 2)
T(llong, 1)
T(double, 8)
T(double, 4)
T(double, 2)
T(double, 1)
