/*
REQUIRED_ARGS: -verrors=3
TEST_OUTPUT:
---
compilable/deprecationlimit.d(18): Deprecation: function `deprecationlimit.f` is deprecated
compilable/deprecationlimit.d(19): Deprecation: function `deprecationlimit.f` is deprecated
compilable/deprecationlimit.d(20): Deprecation: function `deprecationlimit.f` is deprecated
1 deprecation warning omitted, use `-verrors=0` to show all
---
*/

deprecated void f()
{
}

void main()
{
    f();
    f();
    f();
    f();
}
