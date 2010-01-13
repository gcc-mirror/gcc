__extension__ typedef unsigned long long int uint64_t;
typedef uint64_t ScmUInt64;
void swapb64(ScmUInt64 *loc) 
{
    union {
        ScmUInt64 l;
        unsigned char c[4];
    } dd;
    unsigned char t;
    dd.l = *loc;
    (t = dd.c[3], dd.c[3] = dd.c[4], dd.c[4] = t);
}
