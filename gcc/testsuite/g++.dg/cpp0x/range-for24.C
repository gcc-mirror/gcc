// PR c++/56794
// { dg-require-effective-target c++11 }

template<int... values>
static void Colors()
{
    static const int colors[] = { values... };

    for(auto c: colors) { }
}

int main()
{
    Colors<0,1,2> ();
}
