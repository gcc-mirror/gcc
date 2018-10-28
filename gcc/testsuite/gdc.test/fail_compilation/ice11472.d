/*
TEST_OUTPUT:
---
fail_compilation/ice11472.d(13): Error: template instance fun2!fun fun2 is not a template declaration, it is a function
fail_compilation/ice11472.d(18): Error: template instance ice11472.fun1!(fun3) error instantiating
---
*/

void fun3() {}
void fun2(string a) {}
void fun1(alias fun=fun3)()
{
    "a".fun2!fun;
}

void main()
{
    fun1;
}
