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

vector (8, short) __attribute__ ((noinline))
f (vector (8, short) x, vector (8, short) mask) {
    return __builtin_shuffle (x, mask);
}


int main (int argc, char *argv[]) {
    vector (8, short) v0 = {argc, 1,2,3,4,5,6,7};
    vector (8, short) v1 = {argc, 1,argc,3,4,5,argc,7};
    vector (8, short) v2;

    vector (8, short) mask = {0,0,1,2,3,4,5,6};

    v2 = f (v0,  mask);
    shufcompare (short, 8, v2, v0, mask);

    v2 = f (v0, v1);
    shufcompare (short, 8, v2, v0, v1);

    return 0;
}

