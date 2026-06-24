/* { dg-do compile } */
/* { dg-options "-Os -mno-ll64" } */

#ifndef __uncached
#define __uncached __attribute__((uncached))
#endif

volatile char *
pack_byte_ptr (volatile char *p, const void *s, int len)
{
    volatile __uncached char *buf = p + 4;
    for (int i = 0; i < len; i++)
        buf[i] = ((const char *)s)[i];
    return p;
}

volatile short *
pack_halfword_ptr (volatile short *p, const void *s, int len)
{
    volatile __uncached short *buf = p + 4;
    for (int i = 0; i < len; i++)
        buf[i] = ((const short *)s)[i];
    return p;
}

volatile int *
pack_word_ptr (volatile int *p, const void *s, int len)
{
    volatile __uncached int *buf = p + 4;
    for (int i = 0; i < len; i++)
        buf[i] = ((const int *)s)[i];
    return p;
}

volatile long long *
pack_doubleword_ptr (volatile long long *p, const void *s, int len)
{
    volatile __uncached long long *buf = p + 4;
    for (int i = 0; i < len; i++)
        buf[i] = ((const long long *)s)[i];
    return p;
}

/* { dg-final { scan-assembler "stb.*\\.di" } } */
/* { dg-final { scan-assembler "\\m(sth|stw).*\\.di" } } */
/* { dg-final { scan-assembler "st.*\\.di" } } */

/* { dg-final { scan-assembler-not {stb.*\.di\s+r[0-9]+,\s*\[r[0-9]+,\s*r[0-9]+\]} } } */
/* { dg-final { scan-assembler-not {\m(sth|stw).*\.di\s+r[0-9]+,\s*\[r[0-9]+,\s*r[0-9]+\]} } } */
/* { dg-final { scan-assembler-not {st.*\.di\s+r[0-9]+,\s*\[r[0-9]+,\s*r[0-9]+\]} } } */
