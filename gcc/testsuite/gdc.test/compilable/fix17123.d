/*
PERMUTE_ARGS:

https://issues.dlang.org/show_bug.cgi?id=17123
 */

void test()
{
    char[256] buffer;

    char[] delegate() read = () {
        return buffer[];
    };
}
