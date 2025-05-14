// PERMUTE_ARGS:
// REQUIRED_ARGS:

/*
TEST_OUTPUT:
---
print string
print wstring
print dstring
يطبع الترميز الموحد
يطبع الترميز الموحد
يطبع الترميز الموحد
foo_str
foo_wstr
foo_dstr
X%nY
---
*/

pragma(msg, "print string");
pragma(msg, "print wstring"w);
pragma(msg, "print dstring"d);

pragma(msg, "يطبع الترميز الموحد");
pragma(msg, "يطبع الترميز الموحد"w);
pragma(msg, "يطبع الترميز الموحد"d);

void main()
{
    enum a = "foo_str";
    enum b = "foo_wstr"w;
    enum c = "foo_dstr"d;

    pragma(msg, a);
    pragma(msg, b);
    pragma(msg, c);

    // https://github.com/dlang/dmd/issues/20894
    pragma(msg, "X%nY");
}
