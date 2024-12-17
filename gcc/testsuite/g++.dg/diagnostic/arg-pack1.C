// PR c++/118073
// { dg-do compile { target c++11 } }

template<int... Ns>
struct index_sequence {};

void foo()
{
    index_sequence<5> bar = index_sequence<1>(); // { dg-error {conversion from 'index_sequence<\{1\}>' to non-scalar type 'index_sequence<\{5\}>' requested} }
}
