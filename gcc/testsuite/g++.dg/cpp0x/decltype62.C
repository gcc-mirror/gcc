// PR c++/64398
// { dg-do compile { target c++11 } }

template<typename T> struct template1;

template<typename T, typename> // second param required
struct struct1{
	using type1 = decltype(T::x);
	using type2 = template1<type1>;
};

template<typename T> using alias1 = template1<decltype(T::x)>;

// just for instantiation:
template<typename T> using alias2 = alias1<T>;
