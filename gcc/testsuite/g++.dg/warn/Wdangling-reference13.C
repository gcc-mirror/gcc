// PR c++/109774
// { dg-do compile }
// { dg-options "-Wdangling-reference" }

int y;

template<typename T>
int& get(const char& )
{
    return y;
}

int& get2(const char&)
{
    return y;
}

int stuff(void)
{
    const int &h = get<void>(0); // { dg-bogus "dangling reference" }
    const int &k = get2(0); // { dg-bogus "dangling reference" }
    return h+k;
}
