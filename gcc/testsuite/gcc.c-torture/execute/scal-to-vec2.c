#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define vidx(type, vec, idx) (*((type *) &(vec) + idx))

#define operl(a, b, op) (a op b)
#define operr(a, b, op) (b op a)

#define check(type, count, vec0, vec1, num, op, lr) \
do {\
    int __i; \
    for (__i = 0; __i < count; __i++) {\
        if (vidx (type, vec1, __i) != oper##lr (num, vidx (type, vec0, __i), op)) \
            __builtin_abort (); \
    }\
} while (0)

#define veccompare(type, count, v0, v1) \
do {\
    int __i; \
    for (__i = 0; __i < count; __i++) { \
        if (vidx (type, v0, __i) != vidx (type, v1, __i)) \
            __builtin_abort (); \
    } \
} while (0)


long __attribute__ ((noinline)) vlng () {   return (long)42; }
int  __attribute__ ((noinline)) vint () {   return (int) 43; }
short __attribute__ ((noinline)) vsrt () {   return (short)42; }
char __attribute__ ((noinline)) vchr () {    return (char)42; }


int main (int argc, char *argv[]) {
    vector(16, char) c0 = {argc, 1,2,3,4,5,6,7, argc, 1,2,3,4,5,6,7};
    vector(16, char) c1;
    
    vector(8, short) s0 = {argc, 1,2,3,4,5,6,7};
    vector(8, short) s1;

    vector(4, int) i0 = {argc, 1, 2, 3};
    vector(4, int) i1;

    vector(2, long) l0 = {argc, 1};
    vector(2, long) l1;

    c1 = vchr() + c0; check (char, 16, c0, c1, vchr(), +, l);
    
    s1 = vsrt() + s0; check (short, 8, s0, s1, vsrt(), +, l);
    s1 = vchr() + s0; check (short, 8, s0, s1, vchr(), +, l);

    i1 = vint() * i0; check (int, 4, i0, i1, vint(), *, l);
    i1 = vsrt() * i0; check (int, 4, i0, i1, vsrt(), *, l);
    i1 = vchr() * i0; check (int, 4, i0, i1, vchr(), *, l);

    l1 = vlng() * l0; check (long, 2, l0, l1, vlng(), *, l);
    l1 = vint() * l0; check (long, 2, l0, l1, vint(), *, l);
    l1 = vsrt() * l0; check (long, 2, l0, l1, vsrt(), *, l);
    l1 = vchr() * l0; check (long, 2, l0, l1, vchr(), *, l);

    return 0;
}
