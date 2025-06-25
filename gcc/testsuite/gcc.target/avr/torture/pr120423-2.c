/* { dg-do compile } */
/* { dg-additional-options "-ffixed-18 -ffixed-20 -ffixed-22" } */

struct data
{
    int a;
    int b;
    long c;
};

unsigned char val;
unsigned val2;

void func1 (struct data *d)
{
    d->a = 0;
    d->b = 0x100 * val - 1;
}

void func2 (struct data *d)
{
    d->a = 0;
    d->c = 0x10000 * val2 - 1;
}

void func3 (struct data *d)
{
    d->a = 0;
    d->c = 0x1000000 * val - 1;
}
