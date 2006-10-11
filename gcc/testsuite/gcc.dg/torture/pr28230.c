/* { dg-do run } */
/* { dg-options "-fwrapv" } */

void foo( unsigned long long bb, unsigned short tn, unsigned e, unsigned* w );
void foo( unsigned long long bb, unsigned short tn, unsigned e, unsigned* w )
{
        unsigned n = tn + bb;
        do {
                e = (e > n) ? e : *w;
                n -= (e > n) ? n : e;
                if (*w)
                        *w = 0;
        } while ( n );
}
int main()
{
        unsigned w = 0;
        foo( 0, 0, 0, &w );
        return 0;
}
