// PR c++/41127

#define CHAR_BIT 8
enum EE {ee};
typedef unsigned int T;

struct D {
        T : sizeof(unsigned int) * CHAR_BIT; // OK
        EE : sizeof(EE) * CHAR_BIT; // OK
        enum EE : sizeof(EE) * CHAR_BIT; // not OK
        enum EE xxxx : sizeof(EE) * CHAR_BIT; // OK
        T x : sizeof(unsigned int) * CHAR_BIT; // OK
        enum FF {ff} : sizeof(FF) * CHAR_BIT; // OK
} element;

enum EE xx;
EE yy;
