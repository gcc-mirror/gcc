// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/test27a.d
// PERMUTE_ARGS:

import imports.test27a;

int main()
{
    auto v = new myClass!(int)();
    v.func(5);
    return 0;
}

