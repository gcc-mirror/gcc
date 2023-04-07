#include <assert.h>

struct S{
    float a;
};

namespace std
{
    struct test19248_ {int a;}; // Remove when `extern(C++, ns)` is gone
    struct test19248  {int a;};
};

#ifdef __DMC__
// DMC doesn't support c++11
#elif defined (_MSC_VER) && _MSC_VER <= 1800
// MSVC2013 doesn't support char16_t/char32_t
#else
#define TEST_UNICODE
#endif

struct S18784
{
    int i;
    S18784(int n);
};

S18784::S18784(int n) : i(n) {}

#ifdef __DMC__ // DMC doesn't support c++11
template <class>
#else
template <class...>
#endif
struct SPack
{
    int i;
};

typedef SPack<int> SInt;

bool               passthrough(bool                value)     { return value; }
signed char        passthrough(signed char         value)     { return value; }
unsigned char      passthrough(unsigned char       value)     { return value; }
char               passthrough(char                value)     { return value; }
#ifdef TEST_UNICODE
char16_t           passthrough(char16_t            value)     { return value; }
char32_t           passthrough(char32_t            value)     { return value; }
#endif
wchar_t            passthrough(wchar_t             value)     { return value; }
short              passthrough(short               value)     { return value; }
unsigned short     passthrough(unsigned short      value)     { return value; }
int                passthrough(int                 value)     { return value; }
unsigned int       passthrough(unsigned int        value)     { return value; }
long               passthrough(long                value)     { return value; }
unsigned long      passthrough(unsigned long       value)     { return value; }
long long          passthrough(long long           value)     { return value; }
unsigned long long passthrough(unsigned long long  value)     { return value; }
float              passthrough(float               value)     { return value; }
double             passthrough(double              value)     { return value; }
S                  passthrough(S                   value)     { return value; }
std::test19248     passthrough(const std::test19248 value)    { return value; }
std::test19248_    passthrough(const std::test19248_ value)   { return value; }
SInt               passthrough(SInt value)              { return value; }

bool               passthrough_ptr(bool               *value) { return *value; }
signed char        passthrough_ptr(signed char        *value) { return *value; }
unsigned char      passthrough_ptr(unsigned char      *value) { return *value; }
char               passthrough_ptr(char               *value) { return *value; }
#ifdef TEST_UNICODE
char16_t           passthrough_ptr(char16_t           *value) { return *value; }
char32_t           passthrough_ptr(char32_t           *value) { return *value; }
#endif
wchar_t            passthrough_ptr(wchar_t            *value) { return *value; }
short              passthrough_ptr(short              *value) { return *value; }
unsigned short     passthrough_ptr(unsigned short     *value) { return *value; }
int                passthrough_ptr(int                *value) { return *value; }
unsigned int       passthrough_ptr(unsigned int       *value) { return *value; }
long               passthrough_ptr(long               *value) { return *value; }
unsigned long      passthrough_ptr(unsigned long      *value) { return *value; }
long long          passthrough_ptr(long long          *value) { return *value; }
unsigned long long passthrough_ptr(unsigned long long *value) { return *value; }
float              passthrough_ptr(float              *value) { return *value; }
double             passthrough_ptr(double             *value) { return *value; }
S                  passthrough_ptr(S                  *value) { return *value; }
std::test19248     passthrough_ptr(const std::test19248 *value) { return *value; }
std::test19248_    passthrough_ptr(const std::test19248_ *value) { return *value; }
SInt               passthrough_ptr(SInt *value)         { return *value; }

bool               passthrough_ref(bool               &value) { return value; }
signed char        passthrough_ref(signed char        &value) { return value; }
unsigned char      passthrough_ref(unsigned char      &value) { return value; }
char               passthrough_ref(char               &value) { return value; }
#ifdef TEST_UNICODE
char16_t           passthrough_ref(char16_t           &value) { return value; }
char32_t           passthrough_ref(char32_t           &value) { return value; }
#endif
wchar_t            passthrough_ref(wchar_t            &value) { return value; }
short              passthrough_ref(short              &value) { return value; }
unsigned short     passthrough_ref(unsigned short     &value) { return value; }
int                passthrough_ref(int                &value) { return value; }
unsigned int       passthrough_ref(unsigned int       &value) { return value; }
long               passthrough_ref(long               &value) { return value; }
unsigned long      passthrough_ref(unsigned long      &value) { return value; }
long long          passthrough_ref(long long          &value) { return value; }
unsigned long long passthrough_ref(unsigned long long &value) { return value; }
float              passthrough_ref(float              &value) { return value; }
double             passthrough_ref(double             &value) { return value; }
S                  passthrough_ref(S                  &value) { return value; }
std::test19248     passthrough_ref(const std::test19248 &value) { return value; }
std::test19248_    passthrough_ref(const std::test19248_ &value) { return value; }
SInt               passthrough_ref(SInt &value)         { return value; }

namespace ns1
{
    // D: `char*, const(char)**`
    int constFunction1(const char*, const char**) { return 1; }
    // D: `const(char)*, const(char*)*`
    int constFunction2(const char*, const char* const*) { return 2; }
    // D: `const(char*), const(char**)*`
    int constFunction3(const char* const, const char* const* const*) { return 3; }
    // D: `const(char*), const(char***)`
    int constFunction4(const char* const, const char* const* const* const) { return 42; }
};

struct SmallStruct
{
    int i;
    SmallStruct(int); // implemented in D
    SmallStruct(const SmallStruct &);
};
SmallStruct::SmallStruct(const SmallStruct &rhs)
    : i(rhs.i + 10) {}
void smallStructCallBack(SmallStruct p);
void smallStructTest(SmallStruct p)
{
    assert(p.i == 52);

    smallStructCallBack(p);
    assert(p.i == 52);
}

struct Sdtor
{
    static int counter;
    ~Sdtor();
};

Sdtor::~Sdtor() { ++counter; }
int Sdtor::counter = 0;

void consume(Sdtor value) {}

void consume2(Sdtor value);
void doConsume2(Sdtor& value)
{
    consume2(value);
}

// Uncomment when mangling is fixed
// typedef void(*fn0)();
// fn0            passthrough_fn0   (fn0 value) { return value; }
// typedef int (*fn1)(int);
// fn1            passthrough_fn1   (fn1 value) { return value; }

/******************************/

struct FF
{
    float x, y;
    ~FF();
};

float draw(FF min, FF max)
{
    return min.x * 1000 + min.y * 100 + max.x * 10 + max.y;
}

/******************************/

struct FF2
{
    float x, y;
    FF2(int i) { }
};

float draw2(FF2 min, FF2 max)
{
    return min.x * 1000 + min.y * 100 + max.x * 10 + max.y;
}
