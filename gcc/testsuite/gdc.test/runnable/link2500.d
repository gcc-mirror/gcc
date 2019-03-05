// EXTRA_SOURCES: imports/link2500a.d
// EXTRA_SOURCES: imports/link2500b.d
// COMPILE_SEPARATELY:

module link2500;

import imports.link2500a;
import imports.link2500b;

public class A
{
    S!A c;
}

void main()
{
    A a = new A();
    a.c.foo();
}
