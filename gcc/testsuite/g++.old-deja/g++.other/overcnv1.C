// Build don't link:

class A {
public:
	void f(const char * const * );
};
void f(const char * const *) {}

void g()
{
	char *ar[10];
	A a;
	f(ar);
	a.f(ar);
}
