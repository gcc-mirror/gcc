/* { dg-additional-options "-std=gnu89" } */

typedef struct string STR;
typedef struct atbl ARRAY;
struct string {
    unsigned char str_pok;
};
struct atbl {
    int ary_fill;
};
blah(size,strp)
register int size;
register STR **strp;
{
    register ARRAY *ar;
    ar->ary_fill = size - 1;
    while (size--)
     (*strp)->str_pok &= ~128;
}

