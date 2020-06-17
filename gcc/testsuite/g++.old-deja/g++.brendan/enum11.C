// { dg-do assemble  }
// GROUPS passed enums
typedef unsigned uint32_t __attribute__((mode (__SI__)));
class X
{
    enum
    {
       oneMask = 0x0000FFFF,
       twoMask  = 0x000F0000,
       thiMask = 0xFFF00000, // { dg-error "comma at end" "" { target { ! c++11 } } }
    };
    uint32_t foo;

public:
    X (int) : foo (oneMask | twoMask ) {}               // No warning
    X ()    : foo (oneMask | twoMask | thiMask) {}      // Warning
};
