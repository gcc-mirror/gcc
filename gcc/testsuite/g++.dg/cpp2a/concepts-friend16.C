// PR c++/110006
// { dg-do compile { target c++20 } }

template<typename T>
class s;

template<typename T>
void constraint(s<T> const&, int&);

template<typename U, typename T2>
U function(s<T2> const x)
	requires requires (U& u) { constraint(x, u); };

template<typename T>
class s
{
	template<typename U, typename T2>
	friend U function(s<T2> const x)
		requires requires (U& u) { constraint(x, u); };
};

int f(s<int> q)
{
	return function<int>(q); // { dg-bogus "ambiguous" }
}
