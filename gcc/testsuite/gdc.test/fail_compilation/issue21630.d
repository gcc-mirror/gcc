/+
TEST_OUTPUT:
---
fail_compilation/issue21630.d(14): Error: cannot declare `enum` loop variables for non-unrolled foreach
fail_compilation/issue21630.d(15): Error: cannot declare `alias` loop variables for non-unrolled foreach
fail_compilation/issue21630.d(16): Error: cannot declare `enum` loop variables for non-unrolled foreach
fail_compilation/issue21630.d(17): Error: cannot declare `alias` loop variables for non-unrolled foreach
---
+/

void main()
{
    enum a = [1, 2, 3];
    foreach(enum i; a) { } // error
    foreach(alias i; a) { } // error
    foreach(enum i; 0 .. 3) {  } // error
    foreach(alias i; 0 .. 3) {  } // error
}
