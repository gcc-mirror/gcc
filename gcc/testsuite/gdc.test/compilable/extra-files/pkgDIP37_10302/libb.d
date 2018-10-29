module pkgDIP37_10302.libb;
import pkgDIP37_10302.liba;
void bar()
{
    foo();

    // should be error, but unfortunately this works by bug 314 now.
    //lib.foo();

    pkgDIP37_10302.liba.foo();
}
