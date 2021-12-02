/*
REQUIRED_ARGS: -o- -Hf${RESULTS_DIR}/compilable/header18364.di
PERMUTE_ARGS:
OUTPUT_FILES: ${RESULTS_DIR}/compilable/header18364.di

TEST_OUTPUT:
---
=== ${RESULTS_DIR}/compilable/header18364.di
// D import file generated from 'compilable/header18364.d'
module foo.bar.ba;
nothrow pure @nogc @safe package(foo) 
{
	void foo();
	nothrow pure @nogc @safe package(foo.bar) void foo2();
}
---
*/

module foo.bar.ba;
@safe pure nothrow @nogc package(foo):
void foo();

@safe pure nothrow @nogc package(foo.bar):
void foo2();
