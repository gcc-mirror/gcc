/*
TEST_OUTPUT:
----
fail_compilation/test20549.d(12): Error: variable `test.__a_field_0` variables cannot be of type `void`
----
*/

module test;

alias AliasSeq(T...) = T;

enum a = AliasSeq!test;
