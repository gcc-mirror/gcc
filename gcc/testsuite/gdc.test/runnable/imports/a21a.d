module imports.a21a;

import core.stdc.stdio;
import a21;

template GoodMixin()
{
    int goodFunc()
    {
	printf("goodFunc\n");
	return 1;
    }
}


class SomeClass
{
    mixin GoodMixin;
    mixin BadMixin;
}
