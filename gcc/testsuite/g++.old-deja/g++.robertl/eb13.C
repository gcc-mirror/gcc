// Build don't link:
template<class T>
class Array {
public:
    typedef T T_numtype;
};

template<class T_array>
void f(T_array, typename T_array::T_numtype)
{
}

void g()
{
    f(Array<float>(), float());
}
