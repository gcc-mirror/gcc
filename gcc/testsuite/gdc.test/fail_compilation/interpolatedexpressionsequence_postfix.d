/* TEST_OUTPUT:
---
fail_compilation/interpolatedexpressionsequence_postfix.d(10): Error: String postfixes on interpolated expression sequences are not allowed.
fail_compilation/interpolatedexpressionsequence_postfix.d(11): Error: String postfixes on interpolated expression sequences are not allowed.
fail_compilation/interpolatedexpressionsequence_postfix.d(12): Error: String postfixes on interpolated expression sequences are not allowed.
---
*/
void main() {
    // all postfixes are banned
    auto c = i"foo"c;
    auto w = i"foo"w;
    auto d = i"foo"d;
}
