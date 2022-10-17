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
