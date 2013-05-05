// { dg-options "-std=c++0x" }

typedef unsigned volatile long long uvlonglong;

enum E1 : char { };
enum E2 : signed const short { };
enum E3 : uvlonglong { };
enum E4 : char { 
  val = 500 // { dg-error "outside the range" }
};

enum class E5 {
  val = (unsigned long long)-1 // { dg-error "outside the range" }
};

typedef float Float;

enum class E6 : Float { }; // { dg-error "must be an integral type" }

static_assert (sizeof(E1) == sizeof(char), "char-sized enum");
static_assert (sizeof(E2) == sizeof(signed short), "short-sized enum"); 
static_assert (sizeof(E3) == sizeof(unsigned long long), 
               "long long-sized enum"); 
static_assert (sizeof(E4) == sizeof(char), "char-sized enum");
static_assert (sizeof(E5) == sizeof(int), "scoped enum with int size");
