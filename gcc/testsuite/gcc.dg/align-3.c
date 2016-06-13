/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

typedef struct { char a[2]; } __attribute__((__packed__)) TU2;
unsigned short get16_unaligned(const void *p) {
    unsigned short v;
    *(TU2 *)(void *)(&v) = *(const TU2 *)p;
    return v;
}

/* { dg-final { scan-rtl-dump "MEM\[^\n\r\]*A8\\\]" "expand" } } */
