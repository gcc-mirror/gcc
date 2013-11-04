// Bug: We weren't doing the normal replacement of array with pointer
// for deduction in the context of a call because of the initializer list.
// { dg-options "-std=c++11" }

#include <initializer_list>

struct string
{
  string (const char *);
};

template <class T>
struct vector
{
  template <class U>
  vector (std::initializer_list<U>);
};

vector<string> v = { "a", "b", "c" };
