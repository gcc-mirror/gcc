/* { dg-do run } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

#define lparen (
#define a0 fooa0
#define a1  fooa1 a0
#define a2  fooa2 a1
#define a3  fooa3 a2
#define a() b lparen )
#define b() c lparen )
#define c() d lparen )
#define g h
#define i(j) j
#define f(...) #__VA_OPT__(g i(0))
#define k(x,...) # __VA_OPT__(x) #x #__VA_OPT__(__VA_ARGS__)
#define l(x,...) #__VA_OPT__(a1 x)
#define m(x,...) "a()" #__VA_OPT__(a3 __VA_ARGS__ x ## __VA_ARGS__ ## x ## c a3) "a()"
#define n(x,...) = #__VA_OPT__(a3 __VA_ARGS__ x ## __VA_ARGS__ ## x ## c a3) #x #__VA_OPT__(a0 __VA_ARGS__ x ## __VA_ARGS__ ## x ## c a0) ;
#define o(x, ...) #__VA_OPT__(x##x x##x)
#define p(x, ...) #__VA_OPT__(_Pragma ("foobar"))
#define q(...) #__VA_OPT__(/* foo */x/* bar */)
const char *v1 = f();
const char *v2 = f(123);
const char *v3 = k(1);
const char *v4 = k(1, 2, 3 );
const char *v5 = l(a());
const char *v6 = l(a1 a(), 1);
const char *v7 = m();
const char *v8 = m(,);
const char *v9 = m(,a3);
const char *v10 = m(a3,a(),a0);
const char *v11 n()
const char *v12 n(,)
const char *v13 n(,a0)
const char *v14 n(a0, a(),a0)
const char *v15 = o(, 0);
const char *v16 = p(0);
const char *v17 = p(0, 1);
const char *v18 = q();
const char *v19 = q(1);

int
main ()
{
  if (__builtin_strcmp (v1, "")
      || __builtin_strcmp (v2, "g i(0)")
      || __builtin_strcmp (v3, "1")
      || __builtin_strcmp (v4, "112, 3")
      || __builtin_strcmp (v5, "")
      || __builtin_strcmp (v6, "a1 fooa1 fooa0 b ( )")
      || __builtin_strcmp (v7, "a()a()")
      || __builtin_strcmp (v8, "a()a()")
      || __builtin_strcmp (v9, "a()a3 fooa3 fooa2 fooa1 fooa0 a3c a3a()")
      || __builtin_strcmp (v10, "a()a3 b ( ),fooa0 a3a(),a0a3c a3a()")
      || __builtin_strcmp (v11, "")
      || __builtin_strcmp (v12, "")
      || __builtin_strcmp (v13, "a3 fooa0 a0c a3a0 fooa0 a0c a0")
      || __builtin_strcmp (v14, "a3 b ( ),fooa0 a0a(),a0a0c a3a0a0 b ( ),fooa0 a0a(),a0a0c a0")
      || __builtin_strcmp (v15, "")
      || __builtin_strcmp (v16, "")
      || __builtin_strcmp (v17, "_Pragma (\"foobar\")")
      || __builtin_strcmp (v18, "")
      || __builtin_strcmp (v19, "x"))
    __builtin_abort ();
  return 0;
}
