// { dg-do assemble  }
// Origin: Jens Maurer <jmaurer@menuett.rhein-main.de>

template<class T, void (T::*f)(int)>
class C { };

template<class T>
C<T, &T::output> call(T& obj)
{   	return C<T, &T::output>();
}

class Test {
public:
	void output(int);
};

void sub()
{
	Test t;
	call(t);
}

