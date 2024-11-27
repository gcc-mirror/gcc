// PR c++/117788
// { dg-do compile { target c++11 } }

constexpr int arr1[5]{};
constexpr int arr2[5]{};

template<int I>
void f1 (int(*)[arr1 == arr2 ? I : I]) = delete;  // { dg-warning "comparison between two arrays" "" { target { c++20 && c++23_down } } }
// { dg-error "comparison between two arrays" "" { target c++26 } .-1 }

template<int>
void f1 (...) { }

template<int I>
void f2 (int(*)[arr1 > arr2 ? I : 1]) = delete; // { dg-warning "comparison between two arrays" "" { target { c++20 && c++23_down } } }
// { dg-error "comparison between two arrays" "" { target c++26 } .-1 }

template<int>
void f2 (...) { }

void
g ()
{
  f1<0>(nullptr);
  f2<0>(nullptr);
}
