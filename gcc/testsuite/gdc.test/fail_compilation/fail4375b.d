// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375b.d(18): Warning: else is dangling, add { } after condition at fail_compilation/fail4375b.d(14)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

void main() {
    // disallowed
	if (true)
		foreach (i; 0 .. 5)
			if (true)
				assert(5);
    else
        assert(6);
}

