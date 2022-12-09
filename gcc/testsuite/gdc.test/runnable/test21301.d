/* RUN_OUTPUT:
---
int   AAAA0000
int   AAAA0001
int   AAAA0002
int   AAAA0003
float FFFF0004
float FFFF0005
float FFFF0006
float FFFF0007
float FFFF0008
float FFFF0009
float FFFF0010
float FFFF0011
float FFFF0012
float FFFF0013
float FFFF0014
float FFFF0015
int   AAAA0016
int   AAAA0017
int   AAAA0018
int   AAAA0019
int   AAAA0020
int   AAAA0021
---
 */

// https://issues.dlang.org/show_bug.cgi?id=21301

//extern (C)
void func(
    int p0, int[3] p1, float[3] p2, float p3, float p4, float p5, float p6, float p7,
    float p8, float p9, float p10, float p11, int p12, int[2] p13, int p14, int p15, int p16
) {
    print(p0);

    print(p1[0]);
    print(p1[1]);
    print(p1[2]);

    print(p2[0]);
    print(p2[1]);
    print(p2[2]);

    print(p3);
    print(p4);
    print(p5);
    print(p6);
    print(p7);
    print(p8);
    print(p9);
    print(p10);
    print(p11);
    print(p12);
    print(p13[0]);
    print(p13[1]);
    print(p14);
    print(p15);
    print(p16);
}

static if (0)
{
void print(int x);
void print(float x);
}
else
{
import core.stdc.stdio;

union U
{
    int[22] i = [
        0xAAAA_0000,
        0xAAAA_0001,
        0xAAAA_0002,
        0xAAAA_0003,
        0xFFFF_0004,
        0xFFFF_0005,
        0xFFFF_0006,
        0xFFFF_0007,
        0xFFFF_0008,
        0xFFFF_0009,
        0xFFFF_0010,
        0xFFFF_0011,
        0xFFFF_0012,
        0xFFFF_0013,
        0xFFFF_0014,
        0xFFFF_0015,
        0xAAAA_0016,
        0xAAAA_0017,
        0xAAAA_0018,
        0xAAAA_0019,
        0xAAAA_0020,
        0xAAAA_0021,
    ];

    float[22] f;
}

void print(int x) { printf("int   %08X\n", x); }
void print(float x) { printf("float %08X\n", *(cast(int*) &x)); }

int main()
{
    func(U.init.i[0],
         [U.init.i[1], U.init.i[2], U.init.i[3]],
         [U.init.f[4], U.init.f[5], U.init.f[6]],
         U.init.f[7], U.init.f[8], U.init.f[9], U.init.f[10], U.init.f[11], U.init.f[12], U.init.f[13],
         U.init.f[14], U.init.f[15], U.init.i[16],
         [U.init.i[17], U.init.i[18]],
         U.init.i[19], U.init.i[20], U.init.i[21]
    );
    return 0;
}
}
