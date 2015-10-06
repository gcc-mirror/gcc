// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-options "-flto" }

template <class T> class foo {
    foo() { int const bar[2] = {1, 1}; }
};
template class foo<int>;
