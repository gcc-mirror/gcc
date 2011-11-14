// PR c++/30195
// { dg-do run }

template <class T>
struct B
{
    void foo(T) {}
};

template<class T>
struct Out
{
    struct D : B<T>, B<double>
    {
	using B<T>::foo;
	using B<double>::foo;
	void bar() { foo(3); }
    };
};

int main()
{
    Out<int>::D x;
    x.bar();
    return 0;
}
