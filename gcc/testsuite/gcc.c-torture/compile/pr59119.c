extern void *memmove (void *, const void *, __SIZE_TYPE__);
extern void *memset (void *, int, __SIZE_TYPE__);

typedef struct {
    long n_prefix;
    long n_spadding;
} NumberFieldWidths;

void
fill_number(char *buf, const NumberFieldWidths *spec)
{
    if (spec->n_prefix) {
        memmove(buf,
                (char *) 0,
                spec->n_prefix * sizeof(char));
        buf += spec->n_prefix;
    }
    if (spec->n_spadding) {
        memset(buf, 0, spec->n_spadding);
        buf += spec->n_spadding;
    }
}

