// DR 2387
// { dg-do run { target c++14 } }
// { dg-additional-sources "dr2387-aux.cc" }

template <int N>
const int a = N;
template <int N>
static const int b = N;
template <int N>
extern const int c = N;
template <int N>
const volatile int d = N;
template <int N>
const int e = N;
template <>
const int e <43> = 44;

const int *pa = &a <42>;
const int *pb = &b <42>;
const int *pc = &c <42>;
const volatile int *pd = &d <42>;
const int *pe = &e <43>;
