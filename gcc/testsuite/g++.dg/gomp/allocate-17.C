/* { dg-do compile { target c++17 } } */

/* OpenMP allocate directive in constant expressions where execution does not
   pass through the allocation of the variable in the directive.
   Lambdas, both implicit and explicit constexpr,
   in a function and in a function template.

   These cases will be valid if/when OpenMP relaxes restrictions on directives
   in constexpr functions.  It might make sense to only allow this behavior in
   c++23 though.
   
   Constexpr lambdas are only permitted in c++17, it doesn't make sense to test
   anything prior than that.  */

void do_constexpr_lambda()
{
  auto cl = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v = cl(true); /* { dg-error "'do_constexpr_lambda\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */
}

void do_lambda()
{
  auto cl = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v = cl(true); /* { dg-error "'do_lambda\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */
}

template<typename>
void templ_do_constexpr_lambda()
{
  auto cl = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v = cl(true); /* { dg-error "'templ_do_constexpr_lambda<void>\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */
}
template void templ_do_constexpr_lambda<void>();

template<typename>
void templ_do_lambda()
{
  auto cl = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v = cl(true); /* { dg-error "'templ_do_lambda<void>\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */
}
template void templ_do_lambda<void>();

/* Missing cases for generic lambda, and generic lambda in function template.
   They shouldn't behave differently, but for completeness they should be
   added, I'm just not going to spend any more time on this right now.  */
