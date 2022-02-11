// https://issues.dlang.org/show_bug.cgi?id=20656

import core.stdc.stdlib : free, malloc;

@live
void main()
{
    auto p = malloc(1);
    free(p);
    free(p);
}
