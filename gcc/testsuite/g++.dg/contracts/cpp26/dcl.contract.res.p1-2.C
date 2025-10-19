// N5008 :
// dcl.contract.res/p1
// The result-name-introducer of a postcondition-specifier is a declaration. The result-name-introducer introduces
// the identifier as the name of a result binding of the associated function. If a postcondition assertion has a
// result-name-introducer and the return type of the function is cv void, the program is ill-formed. A result
// binding denotes the object or reference returned by invocation of that function. The type of a result binding
// is the return type of its associated function. The optional attribute-specifier-seq of the attributed-identifier in
// the result-name-introducer appertains to the result binding so introduced.
// [Note 1 : An id-expression that names a result binding is a const lvalue (7.5.5.2). — end note]
//
// check that the return value has correct const qualification
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }


#include <type_traits>

bool is_constified(int &){ return false;}

bool is_constified(const int &){ return true;}


int f1(int i)
     pre (is_constified(i))
     pre (std::is_same<decltype(i), int>::value)
     pre (std::is_same<decltype((i)), const int&>::value)

     post (r: is_constified(r))
     post (r: std::is_same<decltype(r), int>::value)
     post (r: std::is_same<decltype((r)), const int&>::value)
     { return i; };

int& f2(int& i)
    pre (is_constified(i))
    pre (std::is_same<decltype(i), int&>::value)
    pre (std::is_same<decltype((i)), const int&>::value)

    post (r: is_constified(r))
    post (r: std::is_same<decltype(r), int&>::value)
    post (r: std::is_same<decltype((r)), const int&>::value)
    {
      static_assert(std::is_same<decltype(i), int&>::value);
      return i;
    }

int* f2(int* i)
    pre (std::is_same<decltype(i), int*>::value)
    pre (std::is_same<decltype((i)),int * const>::value)

    post (r: std::is_same<decltype(r), int *>::value)
    post (r: std::is_same<decltype((r)),int * const>::value)
    { return i;}

int const * f2(int const * i)
    pre (std::is_same<decltype(i), int const *>::value)
    pre (std::is_same<decltype((i)),int const * const>::value)

    post (r: std::is_same<decltype(r), int const *>::value)
    post (r: std::is_same<decltype((r)),int const * const>::value)
    { return i;}

int main()
{

  int i = 4;
  f1(i);
  f2(i);

}
