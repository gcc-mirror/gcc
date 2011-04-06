// PR c++/48369
// { dg-options -std=gnu++0x }

extern "C" int isnan (double);

void f(double d)
{
    bool b = isnan(d);
}
