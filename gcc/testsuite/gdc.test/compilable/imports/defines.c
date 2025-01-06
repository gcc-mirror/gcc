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

#define ADD(a, b) a + b
#define SUB() 3 - 2

#define NO_BODY()
#define NO_BODY_PARAMS(a, b)
#define DO_WHILE() do { } while(0)

#define pr16199_trigger(cond,func,args) _Generic (cond, default: func args)
#define pr16199_skipped1(a) (1)
#define pr16199_skipped2(b) (2)
#define pr16199_ice         0x3

#define M16199Ea(TYPE) (TYPE __x;)
#define M16199E(X,S,M) ({ M16199Ea(S *); })

#define M16199Da(TYPE,VAR) ((TYPE)(VAR))
#define M16199D(X,S,M) ({ int *__x = (X); M16199Da(S *, __x); })
int pr16199d() { return 7; }

#define M16199C(X,S,M) ({ int __x; })
int pr16199c()
{
    return 8;
}

// https://issues.dlang.org/show_bug.cgi?id=24639
#define NEGATIVE_I32 -1
#define NEGATIVE_U32 -2U
#define NEGATIVE_I64 -3LL
#define NEGATIVE_U64 -4LLU
#define NEGATIVE_F32 -5.0f
#define NEGATIVE_F64 -6.0
