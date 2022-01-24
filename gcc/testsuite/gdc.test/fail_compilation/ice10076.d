/*
TEST_OUTPUT:
---
fail_compilation/ice10076.d(18): Error: template instance `getMembersAndAttributesWhere!()` template `getMembersAndAttributesWhere` is not defined
fail_compilation/ice10076.d(23): Error: template instance `ice10076.getValidaterAttrs!string` error instantiating
fail_compilation/ice10076.d(13):        instantiated from here: `validate!string`
---
*/

void main()
{
    string s;
    validate(s);
}

template getValidaterAttrs(T)
{
    alias getMembersAndAttributesWhere!().Elements getValidaterAttrs;
}

void validate(T)(T)
{
    alias getValidaterAttrs!T memberAttrs;
    auto x = memberAttrs.length;
}
