// Origin: PR c++/42697
// { dg-do compile }

template<class Value_t>
class fparser
{
    template<bool Option>
    void eval2(Value_t r[2]);
public:
    void evaltest();
};

template<>
template<bool Option>
void fparser<int>::eval2(int r[2])
{
    struct ObjType {};
}


template<class Value_t>
void fparser<Value_t>::evaltest
    ()
{
    eval2<false>(0);
}

template class fparser<int>;
