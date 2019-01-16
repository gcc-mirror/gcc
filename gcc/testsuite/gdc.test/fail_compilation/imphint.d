/* PERMUTE_ARGS:
---
fail_compilation/imphint.d(14): Error: 'printf' is not defined, perhaps you need to import core.stdc.stdio; ?
fail_compilation/imphint.d(15): Error: 'writeln' is not defined, perhaps you need to import std.stdio; ?
fail_compilation/imphint.d(16): Error: 'sin' is not defined, perhaps you need to import std.math; ?
fail_compilation/imphint.d(17): Error: 'cos' is not defined, perhaps you need to import std.math; ?
fail_compilation/imphint.d(18): Error: 'sqrt' is not defined, perhaps you need to import std.math; ?
fail_compilation/imphint.d(19): Error: 'fabs' is not defined, perhaps you need to import std.math; ?
---
*/

void foo()
{
    printf("hello world\n");
    writeln("hello world\n");
    sin(3.6);
    cos(1.2);
    sqrt(2.0);
    fabs(-3);
}
