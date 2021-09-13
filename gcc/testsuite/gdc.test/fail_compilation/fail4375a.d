// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375a.d(14): Warning: else is dangling, add { } after condition at fail_compilation/fail4375a.d(11)
---
*/

void main() {
	if (true)
		if (false)
			assert(3);
    else
        assert(4);
}

