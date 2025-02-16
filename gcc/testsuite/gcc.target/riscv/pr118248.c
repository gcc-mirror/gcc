/* This does not fault with an x86 cross to risc-v, but does
   with s390 cross to risc-v, probably due to rounding of the
   argument to the alloca call within the risc-v backend.  */
/* { dg-do compile } */
struct char100
{
    char data[100];
};

struct s118248
{
    void **vtbl;
    struct char100 data;
};

void sink(struct char100 *buf);

struct s118248 *pr118248(struct s118248 *pthis)
{
    struct char100 buf;
    sink(&buf);
    pthis->data = buf;
    return pthis;
}


