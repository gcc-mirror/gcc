// Build don't link: 
// GROUPS passed old-sorry
class a {
public:
	int*	foo();
};

a aa;
a* ap;

class b {
public:
	int	ok(int* p =aa.foo());	

  // dump_init should know what to do with this NON_LVALUE_EXPR
	int	f(int* p =ap->foo());		
};

	int
b::ok(int *p) 
{
	return 0;
}

	int
b::f(int *p) 
{			
	return 0;
}
	void
bar()
{
	b b;
	b.ok();
	b.f();
}
