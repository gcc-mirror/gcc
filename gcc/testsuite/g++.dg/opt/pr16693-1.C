// PR middle-end/16693
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort();

unsigned short ret6666(int) {
    return 0x66;
}

typedef enum {
    a   = 0x0, b   = 0x1, c   = 0x2, d   = 0x3, e   = 0x4, f   = 0x5,
    g   = 0x6, h   = 0x7, i   = 0x8, j   = 0x9, k   = 0xa, l   = 0xb,
    m   = 0xc, n   = 0xd, o   = 0xe, p   = 0xf 
} Test_Enum;

int main(void) {
    unsigned char r1;
    r1 = static_cast<Test_Enum>(0xf & ret6666(44));

    if(r1 != 0x6)
        abort();
    return 0;
}

