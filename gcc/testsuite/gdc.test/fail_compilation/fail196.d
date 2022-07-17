/*
TEST_OUTPUT:
---
fail_compilation/fail196.d(27): Error: delimited string must end in `)"`
fail_compilation/fail196.d(27): Error: implicit string concatenation is error-prone and disallowed in D
fail_compilation/fail196.d(27):        Use the explicit syntax instead (concatenating literals is `@nogc`): "foo(xxx)" ~ ";\n    assert(s == "
fail_compilation/fail196.d(28): Error: semicolon needed to end declaration of `s`, instead of `foo`
fail_compilation/fail196.d(27):        `s` declared here
fail_compilation/fail196.d(28): Error: found `");\n\n    s = q"` when expecting `;` following statement
fail_compilation/fail196.d(30): Error: found `";\n    assert(s == "` when expecting `;` following statement
fail_compilation/fail196.d(31): Error: found `");\n\n    s = q"` when expecting `;` following statement
fail_compilation/fail196.d(33): Error: found `{` when expecting `;` following statement
fail_compilation/fail196.d(33): Error: found `}` when expecting `;` following statement
fail_compilation/fail196.d(34): Error: found `foo` when expecting `;` following statement
fail_compilation/fail196.d(34): Error: found `}` when expecting `;` following statement
fail_compilation/fail196.d(36): Error: found `<` when expecting `;` following statement
fail_compilation/fail196.d(37): Error: found `foo` when expecting `;` following statement
fail_compilation/fail196.d(37): Error: found `<` instead of statement
fail_compilation/fail196.d(43): Error: unterminated string constant starting at fail_compilation/fail196.d(43)
fail_compilation/fail196.d(45): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/fail196.d(45): Error: found `End of File` when expecting `}` following compound statement
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
