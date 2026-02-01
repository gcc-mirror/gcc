/* test bitfields for Digital Mars
 * Note that this test is for win32 only
 *
 *
 * DISABLED: win32mscoff win64 linux freebsd osx
 * RUN_OUTPUT:
---
                DM |   MS |  P32 |  P64
T0  =  1 1 ||  1 1 |  1 1 |  1 1 |  1 1
T1  =  2 2 ||  2 2 |  2 2 |  2 2 |  2 2
T2  =  4 4 ||  4 4 |  4 4 |  4 4 |  4 4
T3  = 16 8 || 16 8 | 16 8 |  8 4 |  8 8
T4  = 16 8 || 16 8 | 16 8 | 12 4 | 16 8
T5  = 16 8 || 16 8 | 16 8 |  8 4 |  8 8
S1  =  8 8 ||  8 8 |  8 8 |  4 4 |  8 8
S2  =  4 4 ||  4 4 |  4 4 |  4 4 |  4 4
S3  =  8 4 ||  8 4 |  8 4 |  4 4 |  4 4
S4  =  8 4 ||  8 4 |  8 4 |  4 4 |  4 4
S5  =  8 4 ||  8 4 |  8 4 |  4 4 |  4 4
S6  =  2 2 ||  2 2 |  2 2 |  2 2 |  2 2
S7  = 16 8 || 16 8 | 16 8 |  4 4 |  8 8
S8  =  4 2 ||  4 2 |  4 2 |  2 2 |  2 2
S8A =  4 2 ||  4 2 |  4 2 |  2 2 |  2 2
S8B =  6 2 ||  6 2 |  6 2 |  2 2 |  2 2
S8C =  8 4 ||  8 4 |  8 4 |  4 4 |  4 4
S9  =  4 2 ||  4 2 |  4 2 |  4 2 |  4 2
S10 =  1 1 ||  0 0 |  * * |  0 1 |  0 1
S11 =  1 1 ||  0 0 |  4 1 |  0 1 |  0 1
S12 =  4 4 ||  4 4 |  4 4 |  4 4 |  4 4
S13 =  8 4 ||  8 4 |  8 4 |  8 4 |  8 4
S14 =  8 4 ||  8 4 |  8 4 |  8 4 |  8 4
S15 =  8 4 ||  8 4 |  8 4 |  4 4 |  4 4
S16 =  4 4 ||  0 0 |  4 4 |  4 1 |  4 1
S17 =  4 4 ||  4 4 |  4 4 |  4 4 |  4 4
S18 =  2 1 ||  2 1 |  2 1 |  5 1 |  9 1
A0  = 16 8 || 16 8 | 16 8 | 12 4 | 16 8
A1  = 12 4 || 12 4 | 12 4 | 12 4 | 12 4
A2  = 12 4 || 12 4 | 12 4 | 12 4 | 12 4
A3  = 16 4 || 16 4 | 16 4 | 16 4 | 16 4
A4  = 12 4 || 12 4 | 12 4 |  8 4 |  8 4
A5  =  2 1 ||  2 1 |  2 1 |  2 1 |  2 1
A6  =  4 2 ||  4 2 |  4 2 |  2 2 |  2 2
A7  = 16 4 || 16 4 | 16 4 | 12 4 | 16 8
A8  = 12 4 || 12 4 | 12 4 |  8 4 |  8 8
A9  = 32 8 || 32 8 | 32 8 | 16 4 | 16 8
A10 =  4 2 ||  4 2 |  4 2 |  2 2 |  2 2
A11 = 16 4 || 16 4 | 16 4 | 12 4 | 12 4
S9 = x30200
S14 = x300000201
S15 = x201
S18 = 1 should be 4
A0 = x1
---
 */

import core.stdc.stdio;

int is64bit() { return size_t.sizeof == 8; }  // otherwise assume 32 bit

/*************************************************************/

struct T0  { ubyte x:1; };                       //
struct T1  { short x:1; };                       //
struct T2  { int x:1; };                         //
struct T3  { ubyte a,b,c,d; long x:1; };         //
struct T4  { ubyte a,b,c,d,e,f,g,h; long x:1; }; //
struct T5  { ubyte a,b,c,d,e,f,g; long x:1; };   //
struct S1  { long f:1; };                        //
struct S2  { int x:1; int y:1; };                //
struct S3  { short c; int x:1; uint y:1; };      //
struct S4  { int x:1; short y:1; };              //
struct S5  { short x:1; int y:1; };              //
struct S6  { short x:1; short y:1; };            //
struct S7  { short x:1; int y:1; long z:1; };    //
struct S8  { ubyte a; ubyte b:1; short c:2; };   //
struct S8A { ubyte b:1; short c:2; };            //
struct S8B { ubyte a; short b:1; ubyte c:2; };   //
struct S8C { ubyte a; int b:1; };                //
struct S9  { ubyte a; ubyte b:2; short c:9; };   //
struct S10 { };                                  // sizeof differs from C treatment
struct S11 { int :0; };                          // sizeof differs from C treatment
struct S12 { int :0; int x; };                   //
struct S13 { uint x:12; uint x1:1; uint x2:1; uint x3:1; uint x4:1; int w; }; //
struct S14 { ubyte a; ubyte b:4; int c:30; };    //
struct S15 { ubyte a; ubyte b:2; int c:9; };     //
struct S16 { int :32; };                         // sizeof differs from C treatment
struct S17 { int a:32; };                        //
struct S18 { ubyte a; long :0; ubyte b; };       //
struct A0  { int a; long b:34, c:4; };           //
struct A1  { int a; uint b:11; int c; };         //
struct A2  { int a; uint b:11, c:5, d:16;        //
             int e; };
struct A3  { int a; uint b:11, c:5, :0, d:16;    //
             int e; };
struct A4  { int a:8; short b:7;                 //
             uint c:29; };
struct A5  { ubyte a:7, b:2; };                  //
struct A6  { ubyte a:7; short b:2; };            //
struct A7  { short a:8; int b:16; int c;         //
             ubyte d:7; };
struct A8  { short a:8; int b:16; int :0;        //
             ubyte c:7; };
struct A9  { ushort a:8; int b:16;               //
             uint c:29; long d:9;
             uint e:2, f:31; };
struct A10 { ushort a:8; ubyte b; };             //
struct A11 { ubyte a; int b:5, c:11, :0, d:8;    //
             struct { int ee:8; } };

int main()
{
    /* MS produces identical results for 32 and 64 bit compiles,
     * DM is 32 bit only
     */
    printf("                DM |   MS |  P32 |  P64\n");
    printf("T0  = %2d %d ||  1 1 |  1 1 |  1 1 |  1 1\n", cast(int)T0.sizeof, cast(int)T0.alignof);
    printf("T1  = %2d %d ||  2 2 |  2 2 |  2 2 |  2 2\n", cast(int)T1.sizeof, cast(int)T1.alignof);
    printf("T2  = %2d %d ||  4 4 |  4 4 |  4 4 |  4 4\n", cast(int)T2.sizeof, cast(int)T2.alignof);
    printf("T3  = %2d %d || 16 8 | 16 8 |  8 4 |  8 8\n", cast(int)T3.sizeof, cast(int)T3.alignof);
    printf("T4  = %2d %d || 16 8 | 16 8 | 12 4 | 16 8\n", cast(int)T4.sizeof, cast(int)T4.alignof);
    printf("T5  = %2d %d || 16 8 | 16 8 |  8 4 |  8 8\n", cast(int)T5.sizeof, cast(int)T5.alignof);
    printf("S1  = %2d %d ||  8 8 |  8 8 |  4 4 |  8 8\n", cast(int)S1.sizeof, cast(int)S1.alignof);
    printf("S2  = %2d %d ||  4 4 |  4 4 |  4 4 |  4 4\n", cast(int)S2.sizeof, cast(int)S2.alignof);
    printf("S3  = %2d %d ||  8 4 |  8 4 |  4 4 |  4 4\n", cast(int)S3.sizeof, cast(int)S3.alignof);
    printf("S4  = %2d %d ||  8 4 |  8 4 |  4 4 |  4 4\n", cast(int)S4.sizeof, cast(int)S4.alignof);
    printf("S5  = %2d %d ||  8 4 |  8 4 |  4 4 |  4 4\n", cast(int)S5.sizeof, cast(int)S5.alignof);
    printf("S6  = %2d %d ||  2 2 |  2 2 |  2 2 |  2 2\n", cast(int)S6.sizeof, cast(int)S6.alignof);
    printf("S7  = %2d %d || 16 8 | 16 8 |  4 4 |  8 8\n", cast(int)S7.sizeof, cast(int)S7.alignof);
    printf("S8  = %2d %d ||  4 2 |  4 2 |  2 2 |  2 2\n", cast(int)S8.sizeof, cast(int)S8.alignof);
    printf("S8A = %2d %d ||  4 2 |  4 2 |  2 2 |  2 2\n", cast(int)S8A.sizeof, cast(int)S8A.alignof);
    printf("S8B = %2d %d ||  6 2 |  6 2 |  2 2 |  2 2\n", cast(int)S8B.sizeof, cast(int)S8B.alignof);
    printf("S8C = %2d %d ||  8 4 |  8 4 |  4 4 |  4 4\n", cast(int)S8C.sizeof, cast(int)S8C.alignof);
    printf("S9  = %2d %d ||  4 2 |  4 2 |  4 2 |  4 2\n", cast(int)S9.sizeof,  cast(int)S9.alignof);
    printf("S10 = %2d %d ||  0 0 |  * * |  0 1 |  0 1\n", cast(int)S10.sizeof, cast(int)S10.alignof); // MS doesn't compile
    printf("S11 = %2d %d ||  0 0 |  4 1 |  0 1 |  0 1\n", cast(int)S11.sizeof, cast(int)S11.alignof);
    printf("S12 = %2d %d ||  4 4 |  4 4 |  4 4 |  4 4\n", cast(int)S12.sizeof, cast(int)S12.alignof);
    printf("S13 = %2d %d ||  8 4 |  8 4 |  8 4 |  8 4\n", cast(int)S13.sizeof, cast(int)S13.alignof);
    printf("S14 = %2d %d ||  8 4 |  8 4 |  8 4 |  8 4\n", cast(int)S14.sizeof, cast(int)S14.alignof);
    printf("S15 = %2d %d ||  8 4 |  8 4 |  4 4 |  4 4\n", cast(int)S15.sizeof, cast(int)S15.alignof);
    printf("S16 = %2d %d ||  0 0 |  4 4 |  4 1 |  4 1\n", cast(int)S16.sizeof, cast(int)S16.alignof);
    printf("S17 = %2d %d ||  4 4 |  4 4 |  4 4 |  4 4\n", cast(int)S17.sizeof, cast(int)S17.alignof);
    printf("S18 = %2d %d ||  2 1 |  2 1 |  5 1 |  9 1\n", cast(int)S18.sizeof, cast(int)S18.alignof);
    printf("A0  = %2d %d || 16 8 | 16 8 | 12 4 | 16 8\n", cast(int)A0.sizeof,  cast(int)A0.alignof);
    printf("A1  = %2d %d || 12 4 | 12 4 | 12 4 | 12 4\n", cast(int)A1.sizeof,  cast(int)A1.alignof);
    printf("A2  = %2d %d || 12 4 | 12 4 | 12 4 | 12 4\n", cast(int)A2.sizeof,  cast(int)A2.alignof);
    printf("A3  = %2d %d || 16 4 | 16 4 | 16 4 | 16 4\n", cast(int)A3.sizeof,  cast(int)A3.alignof);
    printf("A4  = %2d %d || 12 4 | 12 4 |  8 4 |  8 4\n", cast(int)A4.sizeof,  cast(int)A4.alignof);
    printf("A5  = %2d %d ||  2 1 |  2 1 |  2 1 |  2 1\n", cast(int)A5.sizeof,  cast(int)A5.alignof);
    printf("A6  = %2d %d ||  4 2 |  4 2 |  2 2 |  2 2\n", cast(int)A6.sizeof,  cast(int)A6.alignof);
    printf("A7  = %2d %d || 16 4 | 16 4 | 12 4 | 16 8\n", cast(int)A7.sizeof,  cast(int)A7.alignof);
    printf("A8  = %2d %d || 12 4 | 12 4 |  8 4 |  8 8\n", cast(int)A8.sizeof,  cast(int)A8.alignof);
    printf("A9  = %2d %d || 32 8 | 32 8 | 16 4 | 16 8\n", cast(int)A9.sizeof,  cast(int)A9.alignof);
    printf("A10 = %2d %d ||  4 2 |  4 2 |  2 2 |  2 2\n", cast(int)A10.sizeof, cast(int)A10.alignof);
    printf("A11 = %2d %d || 16 4 | 16 4 | 12 4 | 12 4\n", cast(int)A11.sizeof, cast(int)A11.alignof);

    {
        S9 s;
        uint x;
        *cast(uint *)&s = 0;
        s.b = 2; s.c = 3;
        x = *cast(uint *)&s;
        printf("S9 = x%x\n", x);
    }
    {
        S14 s = { 1, 2, 3 };
        ulong v;
        *cast(long *)&s = 0;
        s.a = 1;
        s.b = 2;
        s.c = 3;
        v = *cast(ulong *)&s;
        printf("S14 = x%llx\n", v);
    }
    {
        S15 s = { 1,2,3 };
        uint x;
        *cast(uint *)&s = 0;
        s.a = 1; s.b = 2; s.c = 3;
        x = *cast(uint *)&s;
        printf("S15 = x%x\n", x);
    }
    {
        S18 s;
        printf("S18 = %d should be %d\n", cast(int)(&s.b - &s.a), is64bit() ? 8 : 4);
    }
    {
        A0 s;
        long x;
        *cast(long *)&s = 0;
        s.a = 1; s.b = 15;
        x = *cast(long *)&s;
        printf("A0 = x%llx\n", x);
    }

    return 0;
}
