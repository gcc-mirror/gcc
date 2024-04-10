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
