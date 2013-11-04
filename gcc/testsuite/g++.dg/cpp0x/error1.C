// PR c++/34395
// { dg-do compile }
// { dg-options "-std=c++11" }

template<int... N> void foo (int... x[N])	// { dg-message "int \\\[N\\\]\\.\\.\\. x" }
{
  struct A
  {
    A () { x; }		// { dg-error "use of parameter from containing function" }
  };
}
