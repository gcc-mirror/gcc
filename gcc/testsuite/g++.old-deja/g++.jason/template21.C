// { dg-do assemble  }
// Gosh, this works!

template<class T>
struct A
{
    struct B
    {
	void bar();
    };
    struct C { };
};

template<class T> void A<T>::B::bar() { }

template class A<int>;
