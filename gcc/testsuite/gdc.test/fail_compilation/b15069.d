/*
TEST_OUTPUT:
---
fail_compilation/b15069.d(15): Error: template instance `T!int` `T` is not a template declaration, it is a alias
fail_compilation/b15069.d(10): Error: template instance `b15069.Stuff!(Thing!float)` error instantiating
---
*/
void main()
{
	Stuff!(Thing!(float)) s;
}

struct Stuff(T)
{
	T!(int) var;
}

struct Thing(T)
{
	T varling;
}
