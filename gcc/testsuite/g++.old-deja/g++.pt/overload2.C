//Build don't link:
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
	char*		p = Z(c.O); //ERROR - ambiguous c.O
}
