// Build don't link: 
// GROUPS passed redeclaration
class foo
{
public:
    int bar(int a);
};


void bar(int &a);

int foo::bar(int a)  {
    int a = 0;			// Should this be an error ?// ERROR -  declaration.*

    bar(a);
    return 0;
}
