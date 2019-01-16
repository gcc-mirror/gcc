/*
TEST_OUTPUT:
---
fail_compilation/diag12487.d(15): Error: recursive expansion of template instance 'diag12487.recTemplate!int'
fail_compilation/diag12487.d(25): Error: template instance diag12487.recTemplate!int error instantiating
fail_compilation/diag12487.d(18): Error: function diag12487.recFunction CTFE recursion limit exceeded
fail_compilation/diag12487.d(20):        called from here: recFunction(i)
fail_compilation/diag12487.d(18):        1000 recursive calls to function recFunction
fail_compilation/diag12487.d(27):        called from here: recFunction(0)
---
*/

template recTemplate(T)
{
    enum bool recTemplate = recTemplate!T;
}

bool recFunction(int i)
{
    return recFunction(i);
}

void main()
{
    enum bool value1 = recTemplate!int;

    enum bool value2 = recFunction(0);
}
