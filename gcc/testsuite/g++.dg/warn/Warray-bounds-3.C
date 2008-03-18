/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

extern void function(void * x);

struct A {
    long x;
    char d[0];
};


void test(A * a) {
    function((char *)a - 4); /* { dg-bogus "below array bounds" } */
}

