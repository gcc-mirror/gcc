// PR c++/92010
// { dg-do compile { target c++11 } }

template <typename T = char[3]>
void foo(const T t = "; ")
{
}

int main()
{
  foo ();
}

