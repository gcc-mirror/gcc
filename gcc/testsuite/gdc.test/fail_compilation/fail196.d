/*
TEST_OUTPUT:
---
fail_compilation/fail196.d(29): Error: delimited string must end in `)"`
fail_compilation/fail196.d(29): Error: implicit string concatenation is error-prone and disallowed in D
fail_compilation/fail196.d(29):        Use the explicit syntax instead (concatenating literals is `@nogc`): "foo(xxx)" ~ ";\n    assert(s == "
fail_compilation/fail196.d(30): Error: semicolon needed to end declaration of `s`, instead of `foo`
fail_compilation/fail196.d(29):        `s` declared here
fail_compilation/fail196.d(30): Error: found `");\n\n    s = q"` when expecting `;` following statement `foo(xxx)` on line fail_compilation/fail196.d(30)
fail_compilation/fail196.d(32): Error: found `";\n    assert(s == "` when expecting `;` following statement `[foo[xxx]]` on line fail_compilation/fail196.d(32)
fail_compilation/fail196.d(33): Error: found `");\n\n    s = q"` when expecting `;` following statement `foo[xxx]` on line fail_compilation/fail196.d(33)
fail_compilation/fail196.d(35): Error: found `{` when expecting `;` following statement `foo` on line fail_compilation/fail196.d(35)
fail_compilation/fail196.d(35): Error: found `}` when expecting `;` following statement `xxx` on line fail_compilation/fail196.d(35)
fail_compilation/fail196.d(36): Error: found `foo` when expecting `;` following statement `";\n    assert(s == "` on line fail_compilation/fail196.d(35)
fail_compilation/fail196.d(36): Error: found `}` when expecting `;` following statement `xxx` on line fail_compilation/fail196.d(36)
fail_compilation/fail196.d(38): Error: found `<` when expecting `;` following statement `");\n\n    s = q" < foo` on line fail_compilation/fail196.d(36)
fail_compilation/fail196.d(39): Error: found `foo` when expecting `;` following statement `xxx >> ";\n    assert(s == "` on line fail_compilation/fail196.d(38)
fail_compilation/fail196.d(39): Error: found `<` instead of statement
fail_compilation/fail196.d(45): Error: unterminated string constant starting at fail_compilation/fail196.d(45)
fail_compilation/fail196.d(47): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/fail196.d(36):        unmatched `{`
fail_compilation/fail196.d(47): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/fail196.d(28):        unmatched `{`
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
