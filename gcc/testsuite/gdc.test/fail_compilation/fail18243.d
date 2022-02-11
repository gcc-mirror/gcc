/*
EXTRA_FILES: imports/a18243.d
TEST_OUTPUT:
---
fail_compilation/fail18243.d(15): Error: none of the overloads of `isNaN` are callable using argument types `!()(float)`
---
*/

module fail18243;

import imports.a18243;

void main()
{
    bool b = isNaN(float.nan);
}
