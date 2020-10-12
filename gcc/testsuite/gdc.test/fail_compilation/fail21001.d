/*
TEST_OUTPUT:
---
fail_compilation/fail21001.d(12): Error: undefined identifier `Alias`
---
*/

module fail21001;

import imports.fail21001b;

void main() { Alias var; }
