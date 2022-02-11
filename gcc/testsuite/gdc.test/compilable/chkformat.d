// https://issues.dlang.org/show_bug.cgi?id=20643
// https://issues.dlang.org/show_bug.cgi?id=20644
/*
TEST_OUTPUT:
----
compilable/chkformat.d(14): Deprecation: more format specifiers than 0 arguments
----
*/
import core.stdc.stdio;

void main()
{
    // b20643
    printf("%d \n");

    // b20644
    ubyte b;
    printf("%hhu \n", b);

    char c = '-';
    printf("%c", c);

    short s;
    printf("%hd", s);

    printf("%hn", &s);
}
