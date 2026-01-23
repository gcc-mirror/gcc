/* { dg-do run } */
/* { dg-additional-options "-O3 -march=armv9-a+sve2 -msve-vector-bits=128 --param aarch64-autovec-preference=sve-only" } */

int main(int argc, char *argv[]) {

    __attribute__((vector_size((4) * sizeof(int)))) int i0;
    __attribute__((vector_size((4) * sizeof(int)))) int i1;
    __attribute__((vector_size((4) * sizeof(int)))) int ires;
    int i;

    i0 = (__attribute__((vector_size((4) * sizeof(int)))) int){(int)argc, 1, 2,
                                                               10};
    i1 = (__attribute__((vector_size((4) * sizeof(int)))) int){0, 3, 2,
                                                               (int)-23};
    ;
    do {
        ires = (i0 > i1);
        do {
            int __i;
            for (__i = 0; __i < 4; __i++) {
                if ((ires)[__i] != ((i0)[__i] > (i1)[__i] ? -1 : 0)) {
                    __builtin_printf("%i != (("
                                     "%i"
                                     " "
                                     ">"
                                     " "
                                     "%i"
                                     " ? -1 : 0) ",
                                     (ires)[__i], (i0)[__i], (i1)[__i]);
                    __builtin_abort();
                }
            }
        } while (0);
    } while (0);

    return 0;
}
