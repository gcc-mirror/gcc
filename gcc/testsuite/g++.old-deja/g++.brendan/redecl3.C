// { dg-do assemble  }
// GROUPS passed redeclaration
class foo
{
public:
    int bar(int a);
};


void bar(int &a);

int foo::bar(int a)  {
    int a = 0;			// Should this be an error ?// { dg-error "" }  declaration.*

    bar(a);
    return 0;
}
