// { dg-do compile }
// { dg-options "-O2" }

template < typename T > T h2le (T)
{
    T a;
    unsigned short &b = a;
    short c = 0;
    unsigned char (&d)[2] = reinterpret_cast < unsigned char (&)[2] > (c);
    unsigned char (&e)[2] = reinterpret_cast < unsigned char (&)[2] > (b);
    e[0] = d[0];
    return a;
}

void
bar ()
{
    h2le ((unsigned short) 0);
}
