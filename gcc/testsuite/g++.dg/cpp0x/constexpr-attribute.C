// { dg-do compile { target c++11 } }

//A few constexpr's
constexpr int foo() { return __alignof__(int); }

template<typename T>
constexpr int fooT() { return __alignof__(T); }

template<int N>
constexpr int fooN() { return N; }

//Now the attributes

//with normal variables,
int a __attribute__((aligned(foo())));
int b __attribute__((aligned(fooT<int>())));
int c __attribute__((aligned(fooN<__alignof__(int)>())));

//with variables inside a template,
template <typename T>
void fun()
{
    T a __attribute__((aligned(foo())));
    T b __attribute__((aligned(fooT<T>())));
    T c __attribute__((aligned(fooN<__alignof__(T)>())));
    T d __attribute__((aligned(fooT<int>())));
    T e __attribute__((aligned(fooN<__alignof__(int)>())));
}

//instantiate it,
void bar()
{
    fun<int>();
}

//with classes
struct __attribute__((aligned(foo()))) S0
{
    char dummy;
};
S0 s0;

struct __attribute__((aligned(fooT<int>()))) S1
{
    char dummy;
};
S1 s1;

//and class templates
template <typename T>
struct __attribute__((aligned(foo()))) S2
{
    char dummy;
};

S2<int> s2;

template <typename T>
struct __attribute__((aligned(fooT<T>()))) S3
{
    char dummy;
};
S3<int> s3;
