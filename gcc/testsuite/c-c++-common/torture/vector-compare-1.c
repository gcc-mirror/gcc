/* { dg-do run } */
#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

#define check_compare(count, res, i0, i1, op, fmt) \
do { \
    int __i; \
    for (__i = 0; __i < count; __i ++) { \
      if ((res)[__i] != ((i0)[__i] op (i1)[__i] ? -1 : 0)) \
	{ \
            __builtin_printf ("%i != ((" fmt " " #op " " fmt " ? -1 : 0) ", \
			      (res)[__i], (i0)[__i], (i1)[__i]); \
            __builtin_abort (); \
        } \
    } \
} while (0)

#define test(count, v0, v1, res, fmt); \
do { \
    res = (v0 > v1); \
    check_compare (count, res, v0, v1, >, fmt); \
    res = (v0 < v1); \
    check_compare (count, res, v0, v1, <, fmt); \
    res = (v0 >= v1); \
    check_compare (count, res, v0, v1, >=, fmt); \
    res = (v0 <= v1); \
    check_compare (count, res, v0, v1, <=, fmt); \
    res = (v0 == v1); \
    check_compare (count, res, v0, v1, ==, fmt); \
    res = (v0 != v1); \
    check_compare (count, res, v0, v1, !=, fmt); \
} while (0)


int main (int argc, char *argv[]) {
#define INT  int
    vector (4, INT) i0;
    vector (4, INT) i1;
    vector (4, int) ires;
    int i;

    i0 = (vector (4, INT)){(INT)argc, 1,  2,  10};
    i1 = (vector (4, INT)){0, 3, 2, (INT)-23};
    test (4, i0, i1, ires, "%i");
#undef INT

#define INT unsigned int
    vector (4, int) ures;
    vector (4, INT) u0;
    vector (4, INT) u1;

    u0 = (vector (4, INT)){(INT)argc, 1,  2,  10};
    u1 = (vector (4, INT)){0, 3, 2, (INT)-23};
    test (4, u0, u1, ures, "%u");
#undef INT


#define SHORT short
    vector (8, SHORT) s0;
    vector (8, SHORT) s1;
    vector (8, short) sres;

    s0 = (vector (8, SHORT)){(SHORT)argc, 1,  2,  10,  6, 87, (SHORT)-5, 2};
    s1 = (vector (8, SHORT)){0, 3, 2, (SHORT)-23, 12, 10, (SHORT)-2, 0};
    test (8, s0, s1, sres, "%i");
#undef SHORT

#define SHORT unsigned short
    vector (8, SHORT) us0;
    vector (8, SHORT) us1;
    vector (8, short) usres;

    us0 = (vector (8, SHORT)){(SHORT)argc, 1,  2,  10,  6, 87, (SHORT)-5, 2};
    us1 = (vector (8, SHORT)){0, 3, 2, (SHORT)-23, 12, 10, (SHORT)-2, 0};
    test (8, us0, us1, usres, "%u");
#undef SHORT

#define CHAR signed char
    vector (16, CHAR) c0;
    vector (16, CHAR) c1;
    vector (16, signed char) cres;

    c0 = (vector (16, CHAR)){(CHAR)argc, 1,  2,  10,  6, 87, (CHAR)-5, 2, \
                             (CHAR)argc, 1,  2,  10,  6, 87, (CHAR)-5, 2 };

    c1 = (vector (16, CHAR)){0, 3, 2, (CHAR)-23, 12, 10, (CHAR)-2, 0, \
                             0, 3, 2, (CHAR)-23, 12, 10, (CHAR)-2, 0};
    test (16, c0, c1, cres, "%i");
#undef CHAR

#define CHAR unsigned char
    vector (16, CHAR) uc0;
    vector (16, CHAR) uc1;
    vector (16, signed char) ucres;

    uc0 = (vector (16, CHAR)){(CHAR)argc, 1,  2,  10,  6, 87, (CHAR)-5, 2, \
                              (CHAR)argc, 1,  2,  10,  6, 87, (CHAR)-5, 2 };

    uc1 = (vector (16, CHAR)){0, 3, 2, (CHAR)-23, 12, 10, (CHAR)-2, 0, \
                             0, 3, 2, (CHAR)-23, 12, 10, (CHAR)-2, 0};
    test (16, uc0, uc1, ucres, "%u");
#undef CHAR
/* Float comparison.  */
    vector (4, float) f0;
    vector (4, float) f1;
    __typeof (f0 == f1) ifres;

    f0 = (vector (4, float)){(float)argc, 1.,  2.,  10.};
    f1 = (vector (4, float)){0., 3., 2., (float)-23};
    test (4, f0, f1, ifres, "%f");

/* Double comparison.  */
    vector (2, double) d0;
    vector (2, double) d1;
    __typeof (d0 == d1) idres;

    d0 = (vector (2, double)){(double)argc,  10.};
    d1 = (vector (2, double)){0., (double)-23};
    test (2, d0, d1, idres, "%f");


    return 0;
}

