Ä ä;

/*
TEST_OUTPUT:
---
fail_compilation/fail129.d: Error: module fail129 source file must start with BOM or ASCII character, not \xC3
---
*/

class Ä
{
}

void main() {}
