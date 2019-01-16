void foo(alias param)()
{
}

const CONST1 = 1;
const CONST2 = 1;
static assert(&foo!CONST1 !is &foo!CONST2);
static assert(foo!CONST1.mangleof != foo!CONST2.mangleof);

immutable IMM1 = 1;
immutable IMM2 = 1;
static assert(&foo!IMM1 !is &foo!IMM2);
static assert(foo!IMM1.mangleof != foo!IMM2.mangleof);

// Behaves different for manifest constants!
enum ENUM1 = 1;
enum ENUM2 = 1;
static assert(&foo!ENUM1 is &foo!ENUM2);
static assert(foo!ENUM1.mangleof == foo!ENUM2.mangleof);
