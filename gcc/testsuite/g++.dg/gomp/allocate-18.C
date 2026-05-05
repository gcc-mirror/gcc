/* { dg-do compile { target c++17 } } */

/* OpenMP allocate directive in constant expressions where execution does not
   pass through the allocation of the variable in the directive.
   Lambdas, both implicit and explicit constexpr,
   in a function and in a function template.

   These cases will be valid if/when OpenMP relaxes restrictions on directives
   in constexpr functions.  It might make sense to only allow this behavior in
   c++23 though.
   
   Constexpr lambdas are only permitted in c++17, it doesn't make sense to test
   anything prior than that.
   
   NOTE: The error messages for the cases that are not explicitly declared
   constexpr are probably not what we want, we likely want something stating
   that the calls are not usable in a constant expression because of the use of
   an OpenMP directive.  */

void do_constexpr_lambda()
{
  auto cl = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_constexpr_lambda\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  }(true);
}

void do_constexpr_generic_lambda()
{
  auto cl = [](auto b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_constexpr_generic_lambda\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](auto b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  }(true);
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
  constexpr int v0 = cl(true); /* { dg-error "'do_lambda\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  }(true);
}

void do_generic_lambda()
{
  auto cl = [](auto b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_generic_lambda\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](auto b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  }(true);
}

template<typename>
void do_constexpr_lambda_templ_uninstantiated()
{
  auto cl = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  }(true);
}

template<typename>
void do_constexpr_generic_lambda_templ_uninstantiated()
{
  auto cl = [](auto b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](auto b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  }(true);
}

template<typename>
void do_lambda_templ_uninstantiated()
{
  auto cl = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  }(true);
}

template<typename>
void do_generic_lambda_templ_uninstantiated()
{
  auto cl = [](auto b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](auto b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  }(true);
}


template<typename>
void do_constexpr_lambda_templ()
{
  auto cl = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_constexpr_lambda_templ<void>\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](bool b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  }(true);
}
template void do_constexpr_lambda_templ<void>();

template<typename>
void do_constexpr_generic_lambda_templ()
{
  auto cl = [](auto b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_constexpr_lambda_templ<void>\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](auto b) constexpr {
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
    return a;
  }(true);
}
template void do_constexpr_generic_lambda_templ<void>();

template<typename>
void do_lambda_templ()
{
  auto cl = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_lambda_templ<void>\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](bool b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  }(true);
}
template void do_lambda_templ<void>();

template<typename>
void do_generic_lambda_templ()
{
  auto cl = [](auto b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  };
  constexpr int v0 = cl(true); /* { dg-error "'do_generic_lambda_templ<void>\\\(\\\)::<lambda\\\(bool\\\)>' called in a constant expression" "" { xfail *-*-* } } */

  constexpr int v1 = [](auto b){
    if (b)
      return 42;
    int a = 42;
    #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
    return a;
  }(true);
}
template void do_generic_lambda_templ<void>();

/* Missing cases where the lambda/lambda body is dependent on the outer
   template params.  */
