#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define vidx(type, vec, idx) (*((type *) &(vec) + idx))
#define uint unsigned int

int main (int argc, char *argv[]) {
    vector(4, uint) vuint  = { 1,  2,  3,  4};
    vector(4,  int) vint0  = { 1,  1,  1,  1};
    vector(4,  int) vint1  = {-1, -1, -1, -1};

    vector(4,  int) i1, i2, i3;
    vector(4, uint) u1, u2, u3;

    i1 = vint1<< vint0;
    
    if (vidx(int, i1, 0) != ((int)-1 << (int)1))
        __builtin_abort ();
    if (vidx(int, i1, 1) != ((int)-1 << (int)1))
        __builtin_abort ();
    if (vidx(int, i1, 2) != ((int)-1 << (int)1))
        __builtin_abort ();
    if (vidx(int, i1, 3) != ((int)-1 << (int)1))
        __builtin_abort ();

    u1 = vuint << vint0;

    if (vidx(int, u1, 0) != ((uint)1  << (int)1))
        __builtin_abort ();
    if (vidx(int, u1, 1) != ((uint)2  << (int)1))
        __builtin_abort ();
    if (vidx(int, u1, 2) != ((uint)3  << (int)1))
        __builtin_abort ();
    if (vidx(int, u1, 3) != ((uint)4  << (int)1))
        __builtin_abort ();

    
    i2 = vint1 >> vuint;

    if (vidx(int, i2, 0) != ((int)-1  >> (uint)1))
        __builtin_abort ();
    if (vidx(int, i2, 1) != ((int)-1  >> (uint)2))
        __builtin_abort ();
    if (vidx(int, i2, 2) != ((int)-1  >> (uint)3))
        __builtin_abort ();
    if (vidx(int, i2, 3) != ((int)-1  >> (uint)4))
        __builtin_abort ();


    vint1 >>= vuint;
    
    vuint <<= vint0;
    vuint <<= vint1;


    return 0;
}


