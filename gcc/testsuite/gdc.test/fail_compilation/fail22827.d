// https://issues.dlang.org/show_bug.cgi?id=22827
/* TEST_OUTPUT:
---
fail_compilation/fail22827.d(8): Error: `cent` and `ucent` types are obsolete, use `core.int128.Cent` instead
fail_compilation/fail22827.d(9): Error: `cent` and `ucent` types are obsolete, use `core.int128.Cent` instead
---
*/
cent i22827;
ucent j22827;
