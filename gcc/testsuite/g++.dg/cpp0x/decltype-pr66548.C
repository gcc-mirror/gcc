// PR c++/66548 - Invalid class member access expression in decltype
//                sometimes accepted
// { dg-do compile }
// { dg-options "-ftrack-macro-expansion=0" }

#if __cplusplus < 201103L
# define decltype __typeof__
#endif

struct Meow {};

void f ()
{
  decltype (Meow.purr ()) d;   // { dg-error "expected primary-expression" "pr89875" { xfail c++98_only } }
  (void)&d;
}

void g ()
{
  decltype (Meow.purr);        // { dg-error "expected primary-expression" }
}

