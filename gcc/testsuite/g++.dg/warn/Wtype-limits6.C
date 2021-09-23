// PR c++/100161
// { dg-additional-options "-Wtype-limits" }

void f(unsigned);

template<unsigned n>
void g()
{
    for (unsigned i = 0; i < n; i++) { // { dg-bogus "always false" }
        f(i);
    }
}

void h()
{
    g<0>();
}
