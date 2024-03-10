/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

typedef char v32qi __attribute__ ((vector_size (32)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef long long v4di __attribute__((vector_size (32)));

v16qi foo_v16qi (char a, v16qi b)
{
    return (__extension__ (v16qi) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

v8hi foo_v8hi (short a, v8hi b)
{
    return (__extension__ (v8hi) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,}) & b;
}

v4si foo_v4si (int a, v4si b)
{
    return (__extension__ (v4si) {~a, ~a, ~a, ~a}) & b;
}

v2di foo_v2di (long long a, v2di b)
{
    return (__extension__ (v2di) {~a, ~a}) & b;
}

v32qi foo_v32qi (char a, v32qi b)
{
    return (__extension__ (v32qi) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

v16hi foo_v16hi (short a, v16hi b)
{
    return (__extension__ (v16hi) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,
                                   ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a}) & b;
}

v8si foo_v8si (int a, v8si b)
{
    return (__extension__ (v8si) {~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a,}) & b;
}

v4di foo_v4di (long long a, v4di b)
{
    return (__extension__ (v4di) {~a, ~a, ~a, ~a}) & b;
}

/* { dg-final { scan-assembler-times "vpandn" 8 } } */
