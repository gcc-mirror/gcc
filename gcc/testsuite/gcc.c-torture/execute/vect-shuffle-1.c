#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define vidx(type, vec, idx) (*(((type *) &(vec)) + idx))

#define shufcompare(type, count, vres, v0, mask) \
do { \
    int __i; \
    for (__i = 0; __i < count; __i++) { \
        if (vidx(type, vres, __i) != vidx(type, v0, vidx(type, mask, __i))) \
            __builtin_abort (); \
    } \
} while (0)


int main (int argc, char *argv[]) {
    /*vector (8, short) v0 = {argc, 1,2,3,4,5,6,7};
    vector (8, short) v1 = {argc, 1,argc,3,4,5,argc,7};
    vector (8, short) v2;
   
    vector (8, short) smask = {0,0,1,2,3,4,5,6};
    
    v2 = __builtin_shuffle (v0,  smask);
    shufcompare (short, 8, v2, v0, smask);
    v2 = __builtin_shuffle (v0, v1);
    shufcompare (short, 8, v2, v0, v1);
    v2 = __builtin_shuffle (smask, v0);
    shufcompare (short, 8, v2, smask, v0);*/

    vector (4, int) i0 = {argc, 1,2,3};
    vector (4, int) i1 = {argc, 1, argc, 3};
    vector (4, int) i2;

    vector (4, int) imask = {0,3,2,1};

    /*i2 = __builtin_shuffle (i0, imask);
    shufcompare (int, 4, i2, i0, imask);*/
    i2 = __builtin_shuffle (i0, i1);
    shufcompare (int, 4, i2, i0, i1);
    
    i2 = __builtin_shuffle (imask, i0);
    shufcompare (int, 4, i2, imask, i0);
    
    return 0;
}

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define vidx(type, vec, idx) (*(((type *) &(vec)) + idx))

#define shufcompare(type, count, vres, v0, mask) \
do { \
    int __i; \
    for (__i = 0; __i < count; __i++) { \
        if (vidx(type, vres, __i) != vidx(type, v0, vidx(type, mask, __i))) \
            __builtin_abort (); \
    } \
} while (0)


int main (int argc, char *argv[]) {
    /*vector (8, short) v0 = {argc, 1,2,3,4,5,6,7};
    vector (8, short) v1 = {argc, 1,argc,3,4,5,argc,7};
    vector (8, short) v2;
   
    vector (8, short) smask = {0,0,1,2,3,4,5,6};
    
    v2 = __builtin_shuffle (v0,  smask);
    shufcompare (short, 8, v2, v0, smask);
    v2 = __builtin_shuffle (v0, v1);
    shufcompare (short, 8, v2, v0, v1);
    v2 = __builtin_shuffle (smask, v0);
    shufcompare (short, 8, v2, smask, v0);*/

    vector (4, int) i0 = {argc, 1,2,3};
    vector (4, int) i1 = {argc, 1, argc, 3};
    vector (4, int) i2;

    vector (4, int) imask = {0,3,2,1};

    /*i2 = __builtin_shuffle (i0, imask);
    shufcompare (int, 4, i2, i0, imask);*/
    i2 = __builtin_shuffle (i0, i1);
    shufcompare (int, 4, i2, i0, i1);
    
    i2 = __builtin_shuffle (imask, i0);
    shufcompare (int, 4, i2, imask, i0);
    
    return 0;
}

