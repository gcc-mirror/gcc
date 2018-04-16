// PR c++/84791
// { dg-do compile }

typedef int I;

template <int>
void
foo ()
{
  I i;
  #pragma omp parallel reduction (I::I: i)	// { dg-error "'I' is not a class, namespace, or enumeration" "" { target c++11 } }
    ;						// { dg-error "'I' is not a class or namespace" "" { target c++98_only } .-1 }
}

template void foo<0> ();
