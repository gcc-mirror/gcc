// Build don't link: 
// GROUPS passed enums
class X
{
    enum
    {
       oneMask = 0x0000FFFF,
       twoMask  = 0x000F0000,
       thiMask = 0xFFF00000,
    }; // ERROR - comma
    unsigned int foo;

public:
    X (int) : foo (oneMask | twoMask ) {}               // No warning
    X ()    : foo (oneMask | twoMask | thiMask) {}      // Warning
};
