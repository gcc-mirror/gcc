//Build don't link:
namespace x { };

void f(int);

int main()
{
        x();   // ERROR - calling a namespace
        x=4;   // ERROR - assigning to a namespace
	f(x);  // ERROR - passing a namespace as parameter
}

