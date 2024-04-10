/* */

#define CC 'c'
_Static_assert(CC == 'c', "1");

#define I32 3
_Static_assert(I32 == 3, "3");

#define U32 4U
_Static_assert(U32 == 4U, "4");

#define I64 5L
_Static_assert(I64 == 5U, "5");

#define U64 6UL
_Static_assert(U64 == 6UL, "6");

#define F32 7.0f
_Static_assert(F32 == 7.0f, "7");

#define F64 8.0f
_Static_assert(F64 == 8.0, "8");

#define F80 9.0f
_Static_assert(F80 == 9.0L, "9");

#define SSS "hello"
_Static_assert(SSS[0] == 'h', "10");

#define ABC 12
#define GHI (size) abbadabba
#define DEF (ABC + 5)
