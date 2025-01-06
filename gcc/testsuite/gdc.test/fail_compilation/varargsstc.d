/* TEST_OUTPUT:
---
fail_compilation/varargsstc.d(102): Error: variadic parameter cannot have attributes `out`
fail_compilation/varargsstc.d(103): Error: variadic parameter cannot have attributes `ref`
---
 */

#line 100

int printf(const(char)*, const scope shared return ...);
int printf(const(char)*, ref out scope immutable shared return ...);
int printf(const(char)*, ref scope immutable shared return ...);
