// EXTRA_SOURCES: imports/test16348.d
module mypackage.foo;

void bug()
{
    // removing the if-else also removes the segfault
    if (true) {}
    else
    {
        import mypackage.bar;
        auto b = bar();
    }
}
