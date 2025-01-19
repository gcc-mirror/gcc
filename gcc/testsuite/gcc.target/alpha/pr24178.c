/* { dg-do compile } */
/* { dg-options "-mcpu=ev4" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct S {
    long l;
    unsigned char c;
};
unsigned long f(unsigned char *p10) {
    struct S *p = (struct S *) (p10 + 10);
    return p->c;
}

/* { dg-final { scan-assembler "ldl.*,18\\(" } } */
