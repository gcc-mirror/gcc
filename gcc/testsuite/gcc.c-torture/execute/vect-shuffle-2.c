#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define vidx(type, vec, idx) (*(((type *) &(vec)) + idx))

#define shuf2compare(type, count, vres, v0, v1, mask) \
do { \
    int __i; \
    for (__i = 0; __i < count; __i++) { \
        if (vidx(type, vres, __i) != ((vidx(type, mask, __i) < count) ? \
                          vidx(type, v0, vidx(type, mask, __i)) :  \
                          vidx(type, v1, (vidx(type, mask, __i) - count)))) \
            __builtin_abort (); \
        } \
} while (0)


int main (int argc, char *argv[]) {
    vector (8, short) v0 = {5, 5,5,5,5,5,argc,7};
    vector (8, short) v1 = {argc, 1,8,8,4,9,argc,4};
    vector (8, short) v2;

    //vector (8, short) mask = {1,2,5,4,3,6,7};

    vector (8, short) mask0 = {0,2,3,1,4,5,6,7};
    vector (8, short) mask1 = {0,12,3,4,3,0,10,9};

    vector (8, short) mask2 = {0,8,1,9,2,10,3,11};

    v2 = __builtin_shuffle (v0, v1,  mask0);
    shuf2compare (short, 8, v2, v0, v1, mask0);

    v2 = __builtin_shuffle (v0, v1,  mask1);
    shuf2compare (short, 8, v2, v0, v1, mask1);

    v2 = __builtin_shuffle (v0, v1,  mask2);
    shuf2compare (short, 8, v2, v0, v1, mask2);

    v2 = __builtin_shuffle (mask0, mask0,  v0);
    shuf2compare (short, 8, v2, mask0, mask0, v0);

    return 0;
}

