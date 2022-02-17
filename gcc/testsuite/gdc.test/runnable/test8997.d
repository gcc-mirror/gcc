// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/test8997a.d

module test8997;

import imports.test8997a;

void main()
{
    auto a = new A();

    foreach(key; a.foobar.byKey())
    {
    }
}
