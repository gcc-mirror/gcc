// { dg-options "-std=gnu++0x" }
template<typename ... Args>
struct tuple_base {};

template<typename ... Args>
struct tuple : public tuple_base<Args...>
{ 
};

tuple<> zero;
tuple<int> one;
tuple<float, int> two;
