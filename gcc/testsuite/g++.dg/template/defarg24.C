// C++20 P1766R1 - Mitigating minor modules maladies
// { dg-do compile { target c++11 } }

template <int N>
int f1 (int);
template <int N = 42>
int f1 (int);
template <int N>			// { dg-message "original definition appeared here" }
int f2 (int);
template <int N = 42>
int f2 (int);
template <int N = 42>			// { dg-error "redefinition of default argument for 'int N'" }
int f2 (int);
template <int N = 42>			// { dg-message "original definition appeared here" }
int f3 (int);
template <int N = 43>			// { dg-error "redefinition of default argument for 'int N'" }
int f3 (int);
template <typename T>
int f4 (int);
template <typename T = int>
int f4 (int);
namespace A
{
  template <typename T>			// { dg-message "original definition appeared here" }
  int f5 (int);
  template <typename T = int>
  int f5 (int);
  template <typename T = int>		// { dg-message "original definition appeared here" }
  int f6 (int);
}
namespace A
{
  template <typename T = int>		// { dg-error "redefinition of default argument for 'class T'" }
  int f5 (int);
  template <typename T = long>		// { dg-error "redefinition of default argument for 'class T'" }
  int f6 (int);
}
