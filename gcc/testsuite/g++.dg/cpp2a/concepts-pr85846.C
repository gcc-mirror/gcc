// PR c++/85846
// { dg-do compile { target c++20 } }

template<int=0>
concept A = true;

bool i(A<>);

template<class=int>
concept B = true;

bool j(B<>);
