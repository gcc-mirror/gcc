extern double rint(double);
extern double fmod (double, double);
static inline unsigned foo(double d) {
    double a;
    a = rint(d);
    return (unsigned)(fmod(a, (double)0xFFFFFFFF) + ((d - a) * 0xFFFFFFFF));
}
