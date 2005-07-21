// { dg-do compile }
// PR c++/2922

namespace tpl_ {

template<class T>
char test(T);

template<class T>
struct check
{
    static T const t;
    enum { value = 1 == sizeof(test(t)) };
};

double test(int);

}

bool const two_phase_lookup_supported = tpl_::check<int>::value;

int compile_time_assert[two_phase_lookup_supported ? 1 : -1];
