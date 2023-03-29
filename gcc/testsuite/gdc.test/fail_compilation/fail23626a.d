/* TEST_OUTPUT:
---
fail_compilation/fail23626a.d(10): Deprecation: function `fail23626a.ambig` cannot overload `extern(D)` function at fail_compilation/fail23626a.d(9)
fail_compilation/fail23626a.d(13): Deprecation: function `fail23626a.ambigC` cannot overload `extern(C)` function at fail_compilation/fail23626a.d(12)
fail_compilation/fail23626a.d(16): Error: function `fail23626a.ambigCxx(int a)` conflicts with previous declaration at fail_compilation/fail23626a.d(15)
---
*/

extern(D) int ambig(int a) { return 0; }
extern(D) int ambig(int a) @system { return 1; }

extern(C) int ambigC(int a) { return 2; }
extern(C) int ambigC(int a) @system { return 3; }

extern(C++) int ambigCxx(int a) { return 4; }
extern(C++) int ambigCxx(int a) @system { return 5; }
