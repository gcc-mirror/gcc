// Origin: PR c++/42758
// { dg-do compile }

template<class T> struct less {};

template<class T, typename U = less<T> > struct set {};

struct int_less_than {};

void assert_fail (const char*);

int f(const set<int, int_less_than>&)
{
    assert_fail (__PRETTY_FUNCTION__);

}
