/*
TEST_OUTPUT:
---
fail_compilation/fail4958.d(8): Error: enum member `fail4958.FloatEnum.B` has inexact value due to loss of precision
---
*/

enum FloatEnum : float { A = float.max/2, B, C }
