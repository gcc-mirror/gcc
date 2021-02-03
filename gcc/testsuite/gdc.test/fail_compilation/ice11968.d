/*
TEST_OUTPUT:
----
fail_compilation/ice11968.d(9): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/ice11968.d(9): Error: cannot modify string literal `"fail_compilation$?:windows=\\|/$ice11968.d"`
----
*/

void main() {  delete __FILE__  ; }
