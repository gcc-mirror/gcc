extern double foo(double, double);
extern void bar(float*, int*);

void
baz(int* arg)
{
    float tmp = (float)foo(2.0,1.0);
    unsigned i;
    short junk[64];

    for (i=0; i<10; i++, arg++) {
        bar(&tmp, arg);
    }
}
