// { dg-do compile { target c++11 } }

constexpr int
f ()
{
  int a = 42;
  #pragma omp parallel for simd  /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" }  */
  for (int i=0; i < 10; i++)
    a += i;
  return a;
} // { dg-error "not a return-statement" "" { target c++11_down } }

constexpr int
g ()
{
  int a = 42;
  [[omp::sequence(omp::directive(parallel),omp::directive(for))]]  /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" }  */
  for (int i=0; i < 10; i++)
    a += i;
  return a;
} // { dg-error "not a return-statement" "" { target c++11_down } }

constexpr int
h ()
{
  int a = 42;
  #pragma omp allocate(a) align(128)  /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" }  */
  return a;
} // { dg-error "not a return-statement" "" { target c++11_down } }

constexpr int
i ()
{
  int a [[omp::decl(allocate, align(128))]] = 42;  /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" }  */
  return a;
} // { dg-error "not a return-statement" "" { target c++11_down } }



int main() {
  static constexpr int a = f ();  // { dg-error "called in a constant expression" "" { target c++11_down } }
  static constexpr int b = g ();  // { dg-error "called in a constant expression" "" { target c++11_down } }
  static constexpr int c = h ();  // { dg-error "called in a constant expression" "" { target c++11_down } }
  static constexpr int d = i ();  // { dg-error "called in a constant expression" "" { target c++11_down } }
}
