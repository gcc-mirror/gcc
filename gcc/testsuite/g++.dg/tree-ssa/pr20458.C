/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-skip-if "requires hosted libstdc++ for std::locale cdtor" { ! hostedlib } } */

/* The tail call optimization would inapproriately tail call the 
   destructors due to not recognizing a call clobbered variable */
namespace std
{
	class locale
	{
		public:
			locale();
			~locale();
	};
}

struct B
{
	std::locale _M_buf_locale;
	virtual ~B() {}
};

struct C : public B
{
	char *s;
};

void foo ()
{
	C c;
}

int main()
{
	foo ();
	return 0;
}

