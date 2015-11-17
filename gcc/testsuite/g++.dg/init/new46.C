// { dg-do compile }
// { dg-options "-Wall" }

// Test for c++/68308 - [6 Regression] ICE: tree check: expected integer_cst,
//                      have var_decl in decompose, at tree.h:5105

typedef __typeof__ (sizeof 0) size_t;

// Not defined, only referenced in templates that aren't expected
// to be instantiated to make sure they really aren't to verify
// verify c++/68308.
template <class T> void inst_check ();

// Not instantiated (must not be diagnosed).
template <class T>
char* fn1_x () {
    const size_t a = sizeof (T);
    return inst_check<T>() ? new char [a] : 0;
}

// Not instantiated (must not be diagnosed).
template <size_t N>
char* fn2_1_x () {
    return inst_check<char [N]>() ? new char [N] : 0;
}

template <size_t N>
char* fn2_1 () {
    return new char [N];
}

// Not instantiated (must not be diagnosed).
template <size_t M, size_t N>
char* fn2_2_x () {
    return inst_check<char [M][N]>() ? new char [M][N] : 0;
}

template <size_t M, size_t N>
char* fn2_2 () {
    return new char [M][N];   // { dg-error "size of array is too large" }
}

// Not instantiated (must not be diagnosed).
template <class T>
T* fn3_x () {
    const size_t a = sizeof (T);
    return inst_check<T>() ? new T [a] : 0;
}

template <class T>
T* fn3 () {
    const size_t a = sizeof (T);
    return new T [a];         // { dg-error "size of array is too large" }
}


struct S { char a [__SIZE_MAX__ / 8]; };

void foo ()
{
    fn2_1<1>();
    fn2_1<__SIZE_MAX__ / 4>();
    fn2_2<__SIZE_MAX__ / 4, 4>();
    fn3<S>();
}
