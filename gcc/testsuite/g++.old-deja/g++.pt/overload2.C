// { dg-do assemble  }
template<class T>
class C
{
public:
	C<T*> O();
	C<T*> O() const;
};


int
main()
{
	C<char*>	c;
	char*		p = Z(c.O); //{ dg-error "" } ambiguous c.O
}
