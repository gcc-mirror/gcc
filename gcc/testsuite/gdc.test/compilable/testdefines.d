// EXTRA_FILES: imports/defines.c
import imports.defines;

static assert(CC == 'c');
static assert(I32 == 3);
static assert(U32 == 4);
static assert(I64 == 5);
static assert(U64 == 6);

static assert(F32 == 7.0f);
static assert(F64 == 8.0);
static assert(F80 == 9.0L);

static assert(SSS == "hello");

static assert(ABC == 12);
static assert(DEF == 17);

static assert(ADD(3, 4) == 7);
static assert(SUB() == 1);

static assert(pr16199_skipped1(5) == 1);
static assert(pr16199_skipped2(6) == 2);
static assert(pr16199_ice == 3);

static assert(pr16199d() == 7);
static assert(pr16199c() == 8);

// https://issues.dlang.org/show_bug.cgi?id=24639
static assert(NEGATIVE_I32 == -1);
static assert(NEGATIVE_U32 == cast(uint)-2);
static assert(NEGATIVE_I64 == -3);
static assert(NEGATIVE_U64 == cast(ulong)-4L);
static assert(NEGATIVE_F32 == -5f);
static assert(NEGATIVE_F64 == -6);
static assert(is(typeof(NEGATIVE_I32) == int));
static assert(is(typeof(NEGATIVE_U32) == uint));
static assert(is(typeof(NEGATIVE_I64) == long));
static assert(is(typeof(NEGATIVE_U64) == ulong));
static assert(is(typeof(NEGATIVE_F32) == float));
static assert(is(typeof(NEGATIVE_F64) == double));
