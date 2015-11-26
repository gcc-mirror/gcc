// { dg-do compile }
// { dg-options "-O1" }
// { dg-final { scan-assembler-not "abort" } }

typedef __SIZE_TYPE__ size_t;

extern "C" {
    void abort ();
    void* malloc (size_t);
}

struct UDClass {
    static int n;
    UDClass () { ++n; }
    virtual ~UDClass () { --n; }
};

int UDClass::n;

struct POD {
    char buf [sizeof (UDClass)];
};

enum { N = 123 };

#if defined (__arm__) && defined (__ARM_EABI__)
// On ARM EABI the cookie is always 8 bytes as per Section 3.2.2 of
// http://infocenter.arm.com/help/topic/com.arm.doc.ihi0041d/IHI0041D_cppabi.pdf
static const size_t cookie_size = 8;
#else
// On all other targets, the cookie size is the size of size_t
// GCC, and ideally the C++ standard, should provide an API to
// retrieve this constant.)
static const size_t cookie_size = sizeof (size_t);
#endif

inline __attribute__ ((always_inline))
void* operator new[] (size_t n)
{
    // Verify that array new is invoked with an argument large enough
    // for the array and a size_t cookie to store the number of elements.
    // (This holds for classes with user-defined types but not POD types).
 
  if (n != N * sizeof (UDClass) + cookie_size) abort ();
    return malloc (n);
}

inline __attribute__ ((always_inline))
void* operator new[] (size_t n, void *p)
{
    // Verify that the default placement array new is invoked with
    // an argument just large enough for the array (and no cookie),
    // regardless of whether the type is a POD or class with a user
    // defined ctor.
    if (n != N * sizeof (UDClass)) abort ();
    return p;
}

inline __attribute__ ((always_inline))
void* operator new[] (size_t n, POD *p)
{
    // Verify that placement array new overload for a POD type is
    // invoked with an argument large enough for the array and
    // a cookie.
    if (n != N * sizeof (POD)) abort ();
    return p;
}

inline __attribute__ ((always_inline))
void* operator new[] (size_t n, UDClass *p)
{
    // Verify that placement array new overload for a class type with
    // a user-defined ctor and dtor is invoked with an argument large
    // enough for the array and a cookie.
    if (n != N * sizeof (UDClass) + cookie_size) abort ();
    return p;
}

// UDClassllocate a sufficiently large buffer to construct arrays into.
static unsigned char buf [N * N];

POD* test_new_POD ()
{
    // Avoid testing PODs since for those, the global new is invoked
    // without the overhead of a cookie.
    // return new POD [N];
    return 0;
}

POD* test_default_placement_new_POD ()
{
    // Vefify that no overhead is allocated.
    return new (buf) POD [N];
}

POD* test_overloaded_placement_new_POD ()
{
    // Vefify that no overhead is allocated.
    return new ((POD*)buf) POD [N];
}

UDClass* test_new_UDClass ()
{
    // Vefify that space for a cookie is allocated.
    return new UDClass [N];
}

UDClass* test_default_placement_new_UDClass ()
{
    // Vefify that no overhead is allocated.
    return new (buf) UDClass [N];
}

UDClass* test_overloaded_placement_new_UDClass ()
{
    // Vefify that space for a cookie is allocated.
    return new ((UDClass*)buf) UDClass [N];
}
