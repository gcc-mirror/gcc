/*
TEST_OUTPUT:
---
fail_compilation/fail282.d(13): Error: template instance `fail282.Template!500` recursive expansion exceeded allowed nesting limit
---
*/

// https://issues.dlang.org/show_bug.cgi?id=2920
// recursive templates blow compiler stack
// template_class_09.
template Template(int i)
{
    class Class : Template!(i + 1).Class
    {
    }
}

alias Template!(0).Class Class0;
