module imports.tlsa;

import core.stdc.stdio;

int foo()()
{
    static int z = 7;
    return ++z;
}

/*************************************/


int abc4(T)(T t)
{
    static T qqq;		// TLS comdef
    static       T rrr;	// comdef
    static T sss = 8;	// TLS comdat
    static       T ttt = 9;	// comdat
    printf("qqq = %d, rrr = %d, sss = %d, ttt = %d\n", qqq, rrr, sss, ttt);
    qqq += 2;
    rrr += 3;
    sss += 4;
    ttt += 5;
    return t + qqq + rrr + sss + ttt;
}

int bar4()
{
    return abc4(4);
}

/*************************************/
