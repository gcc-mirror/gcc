// { dg-do assemble  }
namespace x { }

void f(int);

int main()
{
        x();   // { dg-error "" } calling a namespace
        x=4;   // { dg-error "" } assigning to a namespace
	f(x);  // { dg-error "" } passing a namespace as parameter
}

