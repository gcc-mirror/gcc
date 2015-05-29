/* { dg-do compile } */
/* { dg-options "-O -fdump-rtl-expand" } */

struct S {
    int l;
    unsigned char c;
};
unsigned long f(unsigned char *p10) {
    struct S *p = (struct S *) (p10 + 10);
    return p->c;
}

/* The p->c memory access should have alignment of 4 bytes.  */

/* { dg-final { scan-rtl-dump "MEM\[^\\n\]*A32" "expand" } } */
