/* https://issues.dlang.org/show_bug.cgi?id=23235
 */

@safe:

void awkk(string[] ppp...)
{
}

void bark(string[] foo...) {
    awkk(foo);
}

void cack(string[] bar...) {
    bark(bar);
}

void test() {
    cack("abc", "def");
}
