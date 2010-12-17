/* { dg-do run } */

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define vidx(type, vec, idx) (*((type *) &(vec) + idx))
#define uchar unsigned char

#define ch14 1,2,3,4
#define ch1  1,1,1,1
#define chm1 -1,-1,-1,-1

int main (int argc, char *argv[]) {
    vector(16, uchar) vuchar  = { ch14, ch14, ch14, ch14};
    vector(16,  char) vchar0  = { ch1, ch1, ch1, ch1};
    vector(16,  char) vchar1  = { chm1, chm1, chm1, chm1};

    vector(16,  char) i1, i2, i3;
    vector(16, uchar) u1, u2, u3;

    i1 = vchar1<< vchar0;
    
    if (vidx(char, i1, 0) != ((char)-1 << (char)1))
        __builtin_abort ();
    if (vidx(char, i1, 1) != ((char)-1 << (char)1))
        __builtin_abort ();
    if (vidx(char, i1, 2) != ((char)-1 << (char)1))
        __builtin_abort ();
    if (vidx(char, i1, 3) != ((char)-1 << (char)1))
        __builtin_abort ();
    u1 = vuchar << vchar0;

    if (vidx(char, u1, 0) != ((uchar)1  << (char)1))
        __builtin_abort ();
    if (vidx(char, u1, 1) != ((uchar)2  << (char)1))
        __builtin_abort ();
    if (vidx(char, u1, 2) != ((uchar)3  << (char)1))
        __builtin_abort ();
    if (vidx(char, u1, 3) != ((uchar)4  << (char)1))
        __builtin_abort ();

    
    i2 = vchar1 >> vuchar;

    if (vidx(char, i2, 0) != ((char)-1  >> (uchar)1))
        __builtin_abort ();
    if (vidx(char, i2, 1) != ((char)-1  >> (uchar)2))
        __builtin_abort ();
    if (vidx(char, i2, 2) != ((char)-1  >> (uchar)3))
        __builtin_abort ();
    if (vidx(char, i2, 3) != ((char)-1  >> (uchar)4))
        __builtin_abort ();
    
    vchar1 >>= vuchar;
    vuchar <<= vchar0;
    vuchar <<= vchar1;

    return 0;
}

