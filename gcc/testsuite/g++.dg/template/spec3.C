// PR c++/3870
// Test that performing a type instantiation in order to match up a
// specialization doesn't clobber last_function_parms.

template <class T>
struct A { typedef int I; };

template <class T>
inline typename T::I
foo (typename T::I, const T*);

template <>
int foo (int i, const A<long>*)
{
    return i + 1;
}
