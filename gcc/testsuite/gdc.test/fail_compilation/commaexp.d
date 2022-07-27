/* REQUIRED_ARGS: -o-
TEST_OUTPUT:
---
fail_compilation/commaexp.d(27): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(39): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(40): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(41): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(42): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(44): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(45): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(56): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(69): Error: using the result of a comma expression is not allowed
fail_compilation/commaexp.d(81): Error: using the result of a comma expression is not allowed
---
*/

class Entry {}
class MyContainerClass { bool append (Entry) { return false; } }

int main () {
    bool ok;
    size_t aggr;
    MyContainerClass mc;

    // https://issues.dlang.org/show_bug.cgi?id=15997
    enum WINHTTP_ERROR_BASE = 4200;
    enum ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = (WINHTTP_ERROR_BASE, + 44);

    // OK
    for (size_t i; i < 5; ++i, i += 1) {}
    for (size_t i; i < 5; ++i, i += 1, i++) {}
    if (!mc)
        mc = new MyContainerClass, mc.append(new Entry);
    if (Object o = cast(Object)mc) {} // Lowering
    ok = true, mc.append(new Entry);
    assert(ok);

    // NOPE
    for (size_t i; i < 5; ++i, i += (i++, 1)) {}
    for (; aggr++, aggr > 5;) {}
    if (Object o = (ok = true, null)) {}
    ok = (true, mc.append(new Entry));
    assert(!ok);
    ok = true, (ok = (true, false));
    return 42, 0;
}


/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=16022

bool test16022()
{
    enum Type { Colon, Comma }
    Type type;
    return type == Type.Colon, type == Type.Comma;
}

bool test16022_structs()
{
    struct A
    {
        int i;
        string s;
    }

    enum Type { Colon = A(0, "zero"), Comma = A(1, "one") }
    Type type;
    return type == Type.Colon, type == Type.Comma;
}

/********************************************/


void bar11(int*, int*) { }

void test11()
{
    static int* p;
    static int i;
    bar11((i,p), &i);
}
