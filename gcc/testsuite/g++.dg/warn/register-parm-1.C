// PR c++/60955
// { dg-options "-Wextra" }

unsigned int erroneous_warning(register int a) {
    if ((a) & 0xff) return 1; else return 0;
}
unsigned int no_erroneous_warning(register int a) {
    if (a & 0xff) return 1; else return 0;
}
