/*
TEST_OUTPUT:
----
fail_compilation/ice11968.d(8): Error: The `delete` keyword is obsolete.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
----
*/

void main() {  delete __FILE__  ; }
