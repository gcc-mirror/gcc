/* Ensure that we don't complain about conflict markers on
   valid template argument lists, valid in C++11 onwards.  */
// { dg-options "-std=c++11" }

template <typename T>
struct foo
{
  T t;
};

foo <foo <foo <foo <foo <foo <foo <int
>>>>>>> f;
// The above line is valid C++11, and isn't a conflict marker
