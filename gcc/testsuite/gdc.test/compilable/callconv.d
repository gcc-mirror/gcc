// PERMUTE_ARGS:

import core.stdc.stdarg;

struct ABC
{
	int x[4];
}

ABC abc;

int x,y,z;

extern (C):
ABC test2v(int xx, int yy, int zz, ...)
{
    x = xx;
    y = yy;
    z = zz;
    return abc;
}

extern (C++):
ABC test3(int xx, int yy, int zz)
{
    x = xx;
    y = yy;
    z = zz;
    return abc;
}

ABC test3v(int xx, int yy, int zz, ...)
{
    x = xx;
    y = yy;
    z = zz;
    return abc;
}

extern (D):
ABC test4(int xx, int yy, int zz)
{
    x = xx;
    y = yy;
    z = zz;
    return abc;
}

ABC test4v(int xx, int yy, int zz, ...)
{
    x = xx;
    y = yy;
    z = zz;
    return abc;
}
