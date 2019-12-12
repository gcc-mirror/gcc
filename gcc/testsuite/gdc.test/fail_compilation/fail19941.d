/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(8): Error: undefined identifier `dne`
---
*/
auto a = new Auto;
class Auto { int field = &dne; }

/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(17): Error: undefined identifier `dne`
---
*/
const c = new Const;
class Const { int field = &dne; }

/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(26): Error: undefined identifier `dne`
---
*/
enum e = new Enum;
class Enum { int field = &dne; }

/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(35): Error: undefined identifier `dne`
---
*/
__gshared g = new Gshared;
class Gshared { int field = &dne; }

/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(44): Error: undefined identifier `dne`
---
*/
immutable i = new Immutable;
class Immutable { int field = &dne; }

/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(53): Error: undefined identifier `dne`
---
*/
shared s = new Shared;
class Shared { int field = &dne; }

/*
TEST_OUTPUT:
---
fail_compilation/fail19941.d(62): Error: undefined identifier `dne`
---
*/
static t = new Static;
class Static { int field = &dne; }
