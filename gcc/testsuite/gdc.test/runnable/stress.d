// PERMUTE_ARGS:

import core.stdc.stdio : printf;
import std.string : splitLines;
import std.utf : toUTF16, toUTF32;

/***********************************************/

void test3()
{
    char[] str;
    str ~= "test"; // segfault
}

/***********************************************/

class A {
private:
    int _i;
public:
    this(int i) { _i = i; }
    int i() { return _i; };
}

class B : A {
private:
    char[] s;
public:
    this(int i) { super(i); }
}

int main()
{
    printf("Testing array of Chars\n");
    CHAR();
    printf("Testing array of WChars\n");
    WCHAR();
    printf("Testing array of DChars\n");
    DCHAR();

    printf("Testing array of Bytes\n");
    BYTE();
    printf("Testing array of UBytes\n");
    UBYTE();
    printf("Testing array of Shorts\n");
    SHORT();
    printf("Testing array of UShorts\n");
    USHORT();
    printf("Testing array of Ints\n");
    INT();
    printf("Testing array of UInts\n");
    UINT();
    printf("Testing array of Longs\n");
    LONG();
    printf("Testing array of ULongs\n");
    ULONG();
    printf("Testing array of Floats\n");
    FLOAT();
    printf("Testing array of Doubles\n");
    DOUBLE();
    printf("Testing array of Reals\n");
    REAL();

    printf("Testing multi-dim array of Chars\n");
    MDCHAR();

    printf("Testing array of Objects\n");
    CLASS();

    test3();
    return 0;
}

void MDCHAR()
{
    const int ITERS = 100;
    alias char typ;
    typ[][] str;

    str.length = ITERS;
    for(int idx = 0; idx < ITERS; idx++) {
        str[idx] = str[idx] ~ "TEST LINE\n";
    }

    if(str.length != ITERS) printf("Length Error: %d\n",str.length);
    if(str[0].length != 10) printf("Length Error: %d\n",str[0].length);
    if(str[ITERS-1].sizeof != (typ[]).sizeof) printf("Size Error: %d\n",str[ITERS-1].sizeof);
    if(str[ITERS-1][0].sizeof != (typ).sizeof) printf("Size Error: %d\n",str[ITERS-1][0].sizeof);

    foreach(s; str) {
        int lstart;
        foreach(int idx, char c; s) {
            if(c == '\n') {
                typ[] t = s[lstart..idx];
                if(t != "TEST LINE") {
                    printf("Error testing character array\n");
                    break;
                }
                lstart = idx + 1;
            }
        }
    }

    typ[] tmp;
    foreach(char[] s; str) {
        tmp = tmp ~ s;
    }

    foreach(s; splitLines(cast(string)tmp)) {
        int lstart;
        foreach(int idx, char c; s) {
            if(c == '\n') {
                if(s[lstart..idx] != "TEST LINE") {
                    printf("Error testing character array\n");
                    break;
                }
                lstart = idx + 1;
            }
        }
    }
}

void CHAR()
{
    const int ITERS = 1000;
    alias char typ;
    typ[] str;

    for(int idx = 0; idx < ITERS; idx++) {
        str = str ~ "TEST LINE\n";
    }

    if(str.length != (ITERS * 10)) printf("Length Error: %d\n",str.length);
    if(str.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",str.sizeof);

    int lstart;
    foreach(int idx, char c; str) {
        if(c == '\n') {
            if(str[lstart..idx] != "TEST LINE") {
                printf("Error testing character array\n");
                break;
            }
            lstart = idx + 1;
        }
    }
}

void WCHAR()
{
    const int ITERS = 1000;
    alias wchar typ;
    typ[] str;

    for(int idx = 0; idx < ITERS; idx++) {
        str = str ~ toUTF16(cast(char[])"TEST LINE\n");
    }

    if(str.length != (ITERS * 10)) printf("Length Error: %d\n",str.length);
    if(str.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",str.sizeof);

    int lstart;
    foreach(int idx, char c; str) {
        if(c == '\n') {
            if(str[lstart..idx] != toUTF16(cast(char[])"TEST LINE")) {
                printf("Error testing character array\n");
                break;
            }
            lstart = idx + 1;
        }
    }
}

void DCHAR()
{
    const int ITERS = 1000;
    alias dchar typ;
    typ[] str;

    for(int idx = 0; idx < ITERS; idx++) {
        str = str ~ toUTF32(cast(char[])"TEST LINE\n");
    }

    if(str.length != (ITERS * 10)) printf("Length Error: %d\n",str.length);
    if(str.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",str.sizeof);

    int lstart;
    foreach(int idx, char c; str) {
        if(c == '\n') {
            if(str[lstart..idx] != toUTF32(cast(char[])"TEST LINE")) {
                printf("Error testing character array\n");
                break;
            }
            lstart = idx + 1;
        }
    }
}

void BYTE()
{
    const int ITERS = 100;
    alias byte typ;
    typ[] a;

    for(typ idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }


    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }
    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void UBYTE()
{
    const int ITERS = 100;
    alias ubyte typ;
    typ[] a;

    for(typ idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }


    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void SHORT()
{
    const int ITERS = 10000;
    alias short typ;
    typ[] a;

    for(typ idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void USHORT()
{
    const int ITERS = 10000;
    alias ushort typ;
    typ[] a;

    for(typ idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void INT()
{
    const int ITERS = 1000000;
    alias int typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }


    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void UINT()
{
    const int ITERS = 1000000;
    alias uint typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }


    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void LONG()
{
    const int ITERS = 1000000;
    alias long typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void ULONG()
{
    const int ITERS = 1000000;
    alias ulong typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void FLOAT()
{
    const int ITERS = 1000000;
    alias float typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void DOUBLE()
{
    const int ITERS = 1000000;
    alias double typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void REAL()
{
    const int ITERS = 1000000;
    alias real typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        a ~= idx;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx] != idx) {
            printf("a Data Error: %d\n",a[idx]);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx] != idx) {
            printf("b Data Error: %d\n",b[idx]);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];


    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx] != idx) {
            printf("c Data Error: %d\n",c[idx]);
            break;
        }
    }
}

void CLASS()
{
    const int ITERS = 1000000;
    alias B typ;
    typ[] a;

    for(int idx = 0; idx < ITERS; idx++) {
        typ tc = new typ(idx);
        a ~= tc;
    }

    if(a.length != ITERS) printf("Length Error: %d\n",a.length);
    if(a.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",a.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(a[idx].i != idx) {
            printf("a Data Error: %d\n",a[idx].i);
            break;
        }
    }

    typ[] b = a[];

    if(b.length != ITERS) printf("Length Error: %d\n",b.length);
    if(b.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",b.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(b[idx].i != idx) {
            printf("b Data Error: %d\n",b[idx].i);
            break;
        }
    }

    typ[] c;
    c = a[0..ITERS/2] ~ b[ITERS/2..$];

    if(c.length != ITERS) printf("Length Error: %d\n",c.length);
    if(c.sizeof != (typ[]).sizeof) printf("Size Error: %d\n",c.sizeof);
    for(int idx = 0; idx < ITERS; idx++) {
        if(c[idx].i != idx) {
            printf("c Data Error: %d\n",c[idx].i);
            break;
        }
    }
}
