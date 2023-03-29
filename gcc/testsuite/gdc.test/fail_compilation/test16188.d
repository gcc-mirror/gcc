/* REQUIRED_ARGS: -preview=bitfields
 * TEST_OUTPUT:
---
fail_compilation/test16188.d(101): Error: no property `name` for `Where()` of type `test16188.Where`
fail_compilation/test16188.d(101):        potentially malformed `opDispatch`. Use an explicit instantiation to get a better error message
---
 */

// https://issues.dlang.org/show_bug.cgi?id=16188

/* This produces the message:
 *   Error: no property 'name' for type 'Where'
 * when the actual error is 'getMember is undefined'.
 * This happens because errors are gagged when opDispatch() is compiled,
 * I don't understand why.
 */

#line 100

void where() { Where().name; }

struct Where
{
    void opDispatch(string name)()
    {
        alias FieldType = typeof(getMember);
        WhereField!FieldType;
    }
}

struct WhereField(FieldType) {}
