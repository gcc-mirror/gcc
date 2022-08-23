/*
TEST_OUTPUT:
----
fail_compilation/ice11968.d(9): Error: the `delete` keyword is obsolete
fail_compilation/ice11968.d(9):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
----
*/

void main() {  delete __FILE__  ; }
