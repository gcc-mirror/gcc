/* TEST_OUTPUT:
---
fail_compilation/varargsstc.d(102): Error: variadic parameter cannot have attributes `out`
fail_compilation/varargsstc.d(103): Error: variadic parameter cannot have attributes `ref`
---
 */

#line 100

int printf(const(char)*, const return scope shared ...);
int printf(const(char)*, return ref out scope immutable shared ...);
int printf(const(char)*, return ref scope immutable shared ...);
