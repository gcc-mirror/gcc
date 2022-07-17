module object;

alias size_t = typeof(int.sizeof);
class Object {}
auto opEquals(Object ) { return true; }
class TypeInfo {}
class TypeInfo_Const {}
bool _xopEquals() { return true; }

bool __equals(T1, T2)(T1[] lhs, T2[] rhs)
{
    static at(R)(R[] r, size_t i) { return r.ptr[i]; }
    foreach (u; 0 .. lhs.length)
        if (at(lhs, u) != at(rhs, u))
            return false;
    return true;
}
