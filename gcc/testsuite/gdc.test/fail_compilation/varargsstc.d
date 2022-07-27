/* TEST_OUTPUT:
---
fail_compilation/varargsstc.d(102): Error: variadic parameter cannot have attributes `out ref`
---
 */

#line 100

int printf(const(char)*, const scope shared return ...);
int printf(const(char)*, ref out scope immutable shared return ...);
