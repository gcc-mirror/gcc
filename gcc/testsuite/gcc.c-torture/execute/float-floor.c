
#if(__SIZEOF_DOUBLE__==8)
double d = 1024.0 - 1.0 / 32768.0;
#else
double d = 1024.0 - 1.0 / 16384.0;
#endif

extern double floor(double);
extern float floorf(float);
extern void abort();

int main() {

    double df = floor(d);
    float f1 = (float)floor(d);

    if ((int)df != 1023 || (int)f1 != 1023)
      abort ();

    return 0;
}
