// { dg-do compile }

// Origin: Alexandre Tolmos <ktulu@free.fr>

// PR c++/11876: Friend of its own class diagnostics

template <typename T>
class A
{
	friend class A<int>;
	friend class A<float>;
protected:
	T _data;
	inline A() : _data(0) {}
	template <typename U>
	inline A(const A<U>& r) : _data(r._data) {}
};

class B : public A<int>
{
public:
	inline B() {}
	inline B(const B& r) : A<int>(r) {}
};

class C : public A<float>
{
public:
	inline C() {}
	inline C(const B& r) : A<float>(r) {}
};

int main(int, char*[])
{
	B b1, b2(b1);
	C c(b1);
	return 0;
}
