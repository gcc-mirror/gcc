// PR c++/92909
// { dg-do compile { target c++11 } }

template <class ... Ts>
void foo()
{
    []
    {
        using T = Ts;
    }();			// { dg-error "not expanded" }
}
template void foo<>();
