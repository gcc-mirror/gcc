// Build don't link: 
// GROUPS passed enums
template<class T>
struct templ
{
    enum { val = 0 };
};
struct Foo
{
    enum {
	bar = 0,
	len = templ<int>::val
    };
};
void func()
{
    int s = Foo::bar;	// Ensure that expansion of templ did not erase bar
}
