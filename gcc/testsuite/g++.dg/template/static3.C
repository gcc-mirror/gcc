template <class data> class foo
{
    public:
	static const int a;
	static const int b;
	static const int c;
	static const int d;
};

template <class data> const int foo<data>::a = 1;
template <class data> const int foo<data>::b = a;
template <class data> const int foo<data>::c = b;
template <class data> const int foo<data>::d = c;

typedef foo<int> fooInt;

int main( void )
{
    fooInt *f;

    f = new fooInt();

    if (f->c != 1 || f->d != 1)
      return 1;
}
