// PR c++/42055

template<typename T> void foo(T, T); // { dg-message "candidates|template" }

template<typename T> void foo(T, int); // { dg-message "template" }

template void foo(int, int); // { dg-error "ambiguous template specialization" }
