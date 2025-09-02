// C++20 P1766R1 - Mitigating minor modules maladies
// { dg-do compile }

int f1 (int);
int f1 (int = 42);
int f2 (int);
int f2 (int = 42);		// { dg-message "previous specification in 'int f2\\\(int\\\)' here" }
int f2 (int = 42);		// { dg-error "default argument given for parameter 1 of 'int f2\\\(int\\\)'" }
int f3 (int = 42);		// { dg-message "previous specification in 'int f3\\\(int\\\)' here" }
int f3 (int = 43);		// { dg-error "default argument given for parameter 1 of 'int f3\\\(int\\\)'" }
namespace A
{
  int f4 (int = 1);		// { dg-message "previous specification in 'int A::f4\\\(int\\\)' here" }
  int f5 (int = 1);		// { dg-message "previous specification in 'int A::f5\\\(int\\\)' here" }
}
namespace A
{
  int f4 (int = 1);		// { dg-error "default argument given for parameter 1 of 'int A::f4\\\(int\\\)'" }
  int f5 (int = 2);		// { dg-error "default argument given for parameter 1 of 'int A::f5\\\(int\\\)'" }
}
template <int N>
int f6 (long = 42L);
template <int N>
int f6 (long = 42L);		// { dg-error "redeclaration of 'template<int N> int f6\\\(long int\\\)' may not have default arguments" }

void
foo ()
{
  int f7 (int = 42);		// { dg-message "previous specification in 'int f7\\\(int\\\)' here" }
  int f7 (int = 42);		// { dg-error "default argument given for parameter 1 of 'int f7\\\(int\\\)'" }
  int f8 (int = 42);
  {
    int f8 (int = 42);
    {
      int f8 (int = 43);
    }
  }
}
