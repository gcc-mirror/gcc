/*
TEST_OUTPUT:
---
fail_compilation/fail196.d(26): Error: delimited string must end in )"
fail_compilation/fail196.d(26): Error: Implicit string concatenation is error-prone and disallowed in D
fail_compilation/fail196.d(26):        Use the explicit syntax instead (concatenating literals is `@nogc`): "foo(xxx)" ~ ";\x0a    assert(s == "
fail_compilation/fail196.d(27): Error: semicolon expected, not `foo`
fail_compilation/fail196.d(27): Error: found `");\x0a\x0a    s = q"` when expecting `;` following statement
fail_compilation/fail196.d(29): Error: found `";\x0a    assert(s == "` when expecting `;` following statement
fail_compilation/fail196.d(30): Error: found `");\x0a\x0a    s = q"` when expecting `;` following statement
fail_compilation/fail196.d(32): Error: found `{` when expecting `;` following statement
fail_compilation/fail196.d(32): Error: found `}` when expecting `;` following statement
fail_compilation/fail196.d(33): Error: found `foo` when expecting `;` following statement
fail_compilation/fail196.d(33): Error: found `}` when expecting `;` following statement
fail_compilation/fail196.d(35): Error: found `<` when expecting `;` following statement
fail_compilation/fail196.d(36): Error: found `foo` when expecting `;` following statement
fail_compilation/fail196.d(36): Error: found `<` instead of statement
fail_compilation/fail196.d(42): Error: unterminated string constant starting at fail_compilation/fail196.d(42)
fail_compilation/fail196.d(44): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/fail196.d(44): Error: found `End of File` when expecting `}` following compound statement
---
*/

void main()
{
    string s = q"(foo(xxx)) ";
    assert(s == "foo(xxx)");

    s = q"[foo[xxx]]";
    assert(s == "foo[xxx]");

    s = q"{foo{xxx}}";
    assert(s == "foo{xxx}");

    s = q"<foo<xxx>>";
    assert(s == "foo<xxx>");

    s = q"[foo(]";
    assert(s == "foo(");

    s = q"/foo]/";
    assert(s == "foo]");
}
