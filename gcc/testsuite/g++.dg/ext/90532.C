// { dg-do compile { target c++11 } }
// PR c++/90532
static_assert( !__is_constructible(int[]), "" );
static_assert( !__is_constructible(int[], int), "" );
static_assert( !__is_constructible(int[], int[]), "" );
static_assert( !__is_trivially_constructible(int[]), "" );
static_assert( !__is_trivially_constructible(int[], int), "" );
static_assert( !__is_trivially_constructible(int[], int[]), "" );
static_assert( !__is_trivially_constructible(int[], int(&)[]), "" );
static_assert( !__is_trivially_constructible(int[], void), "" );
struct A { };
static_assert( !__is_constructible(A[]), "" );
static_assert( !__is_constructible(A[], const A&), "" );
static_assert( !__is_constructible(A[], const A[]), "" );
static_assert( !__is_trivially_constructible(A[]), "" );
static_assert( !__is_trivially_constructible(A[], const A&), "" );
static_assert( !__is_trivially_constructible(A[], const A[]), "" );
static_assert( !__is_trivially_constructible(A[], A(&)[]), "" );
static_assert( !__is_trivially_constructible(A[], void), "" );
struct B { B(); };
static_assert( !__is_constructible(B[]), "" );
static_assert( !__is_constructible(B[], const B&), "" );
static_assert( !__is_trivially_constructible(B[]), "" );
static_assert( !__is_trivially_constructible(B[], const B&), "" );
static_assert( !__is_trivially_constructible(B[], const B[]), "" );
static_assert( !__is_trivially_constructible(B[], B(&)[]), "" );
static_assert( !__is_trivially_constructible(B[], void), "" );
