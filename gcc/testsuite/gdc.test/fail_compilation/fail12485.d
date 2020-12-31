void dorecursive()
{
    recursive!"ratherLongSymbolNameToHitTheMaximumSymbolLengthEarlierThanTheTemplateRecursionLimit_";
}

void recursive(string name)()
{
    struct S {} // define type to kick off mangler
    static if (name.length <= (4 << 20))
        recursive!(name ~ name);
}

