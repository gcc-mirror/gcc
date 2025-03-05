// check that the return value has correct const qualification
// { dg-do run }
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr " }


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
