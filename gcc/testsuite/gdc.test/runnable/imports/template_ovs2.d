module imports.template_ovs2;

/***************************************************/
// 1900 - template overload set

auto foo1900a(T)(T arg) if (is(T : const(char)[])) { return 2; }
auto foo1900b(int num) { return 1; }

auto bar1900a(T)(T arg) if (is(T : const(char)[])) { return 2; }
auto bar1900b(T)(T arg) if (is(T : double)) { return 1; }

auto baz1900(T)(T arg) if (is(T : Object)) { return 3; }
auto baz1900(T)(T arg) if (is(T : const(char)[])) { return 4; }

auto bad1900(string op)() if (op == "++") { return 2; }

mixin template Mix1900_B()
{
    auto mixfooa(T)(T) { return 2; }
    auto mixfoob() { return 1; }

    mixin Mix1900_SubB;
}

mixin template Mix1900_SubB()
{
    auto mixsubfooa(T)(T) { return 2; }
    auto mixsubfoob() { return 1; }
}

auto merge1900(T)(string)
{
    return 2;
}

/***************************************************/
// 1900

class BClass1900 {}
template Traits1900(T : BClass1900) { enum name = "BClass"; }
string func1900(BClass1900 b) { return "BClass"; }

void Value1900a() {}
template Value1900a(T) if (is(T == string)) { enum Value1900a = 2; }

template Value1900b(T) if (is(T == string)) { enum Value1900b = 2; }
void Value1900b() {}

/***************************************************/
// 8352

void remove8352a(in char[] name) {}
Range remove8352b(alias pred, Range)(Range range) { return range; }

/***************************************************/
// 10658

template Val10658(long n) { enum Val10658 = 2; }
