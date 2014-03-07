// PR c++/48369

extern "C" int isnan (double);

void f(double d)
{
    bool b = isnan(d);
}
