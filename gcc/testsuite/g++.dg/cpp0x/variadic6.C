// { dg-options "-std=gnu++11" }
template<typename ... Args>
struct tuple_base {};

template<typename ... Args>
struct tuple : public tuple_base<Args...>
{ 
};

tuple<> zero;
tuple<int> one;
tuple<float, int> two;
