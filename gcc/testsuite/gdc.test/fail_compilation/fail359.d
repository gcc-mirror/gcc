/*
TEST_OUTPUT:
---
fail_compilation/fail359.d(7): Error: invalid filename for `#line` directive
---
*/
#line 5 _BOOM
void main() { }
