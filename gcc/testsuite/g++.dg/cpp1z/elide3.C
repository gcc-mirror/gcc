// PR c++/85215
// { dg-do compile { target c++11 } }

template <typename _Tp> struct vector {
  vector(vector &&) noexcept;
};

template <typename T> struct any_container {
  operator vector<T> &&();
};

void f (any_container<int> c)
{
  vector<int> shape (c);
}
