/* { dg-options "-fwrapv-pointer" } */

int
callee_ptr(const char *p) {
    return (p + 16 < p);
}
