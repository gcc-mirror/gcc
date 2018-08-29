#ifdef PR83487_LARGE
struct __attribute__ ((aligned (128))) A {};
struct B {};
struct C { struct B c[128]; };
#else
struct __attribute__ ((aligned (16))) A {};
struct B {};
struct C { struct B c[16]; };
#endif
