/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long long int int64_t;
typedef unsigned char uint8_t;
typedef short unsigned int uint16_t;
typedef unsigned int uint32_t;
typedef long long unsigned int uint64_t;

void __attribute__ ((noinline, noclone))
test_1_TYPE1_uint32_t (uint16_t *__restrict f, uint32_t *__restrict d,
                       uint16_t x, uint16_t x2, uint32_t y, int n)
{
    for (int i = 0; i < n; ++i)
        {
            f[i * 2 + 0] = x;
            f[i * 2 + 1] = x2;
            d[i] = y;
        }
}

void __attribute__ ((noinline, noclone))
test_1_TYPE1_int64_t (int32_t *__restrict f, int64_t *__restrict d, int32_t x,
                      int32_t x2, int64_t y, int n)
{
    for (int i = 0; i < n; ++i)
        {
            f[i * 2 + 0] = x;
            f[i * 2 + 1] = x2;
            d[i] = y;
        }
}

int
main (void)
{
        // This part is necessary for ice to appear though running it by itself does not trigger an ICE
        int n_3_TYPE1_uint32_t = 32;
        uint16_t x_3_uint16_t = 233;
        uint16_t x2_3_uint16_t = 78;
        uint32_t y_3_uint32_t = 1234;
        uint16_t f_3_uint16_t[33 * 2 + 1] = { 0} ;
        uint32_t d_3_uint32_t[33] = { 0} ;
        test_1_TYPE1_uint32_t (f_3_uint16_t, d_3_uint32_t, x_3_uint16_t, x2_3_uint16_t, y_3_uint32_t, n_3_TYPE1_uint32_t);
        for (int i = 0;
                        i < n_3_TYPE1_uint32_t;
                        ++i) {
                if (f_3_uint16_t[i * 2 + 0] != x_3_uint16_t) __builtin_abort ();
                if (f_3_uint16_t[i * 2 + 1] != x2_3_uint16_t) __builtin_abort ();
                if (d_3_uint32_t[i] != y_3_uint32_t) __builtin_abort ();
        }
        for (int i = n_3_TYPE1_uint32_t;
                        i < n_3_TYPE1_uint32_t + 1;
                        ++i) {
                if (f_3_uint16_t[i * 2 + 0] != 0) __builtin_abort ();
                if (f_3_uint16_t[i * 2 + 1] != 0) __builtin_abort ();
                if (d_3_uint32_t[i] != 0) __builtin_abort ();
        }
    // If ran without the above section, a different ice appears. see below
    int n_3_TYPE1_int64_t = 32;
    int32_t x_3_int32_t = 233;
    int32_t x2_3_int32_t = 78;
    int64_t y_3_int64_t = 1234;
    int32_t f_3_int32_t[33 * 2 + 1] = { 0 };
    int64_t d_3_int64_t[33] = { 0 };
    test_1_TYPE1_int64_t (f_3_int32_t, d_3_int64_t, x_3_int32_t, x2_3_int32_t,
                          y_3_int64_t, n_3_TYPE1_int64_t);
    for (int i = 0; i < n_3_TYPE1_int64_t; ++i)
        {
            if (f_3_int32_t[i * 2 + 0] != x_3_int32_t)
                __builtin_abort ();
            if (f_3_int32_t[i * 2 + 1] != x2_3_int32_t)
                __builtin_abort ();
            if (d_3_int64_t[i] != y_3_int64_t)
                __builtin_abort ();
        }

    for (int i = n_3_TYPE1_int64_t; i < n_3_TYPE1_int64_t + 1; ++i)
        {
            if (f_3_int32_t[i * 2 + 0] != 0)
                __builtin_abort ();
            if (f_3_int32_t[i * 2 + 1] != 0)
                __builtin_abort ();
            if (d_3_int64_t[i] != 0)
                __builtin_abort ();
        }

    return 0;
}
