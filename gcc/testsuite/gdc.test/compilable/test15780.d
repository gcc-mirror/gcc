// PERMUTE_ARGS:
// https://issues.dlang.org/show_bug.cgi?id=15780

void foo(alias fields)() {
    foreach(i, field; fields) {
        enum string a = fields[i];  // OK
        enum string b = field;      // not OK with 2.069.2 ???
    }
}

void main() {
    foo!(tuple("H", "I"))();
}

Tuple!T tuple(T...)(T values)
{
    return Tuple!T(values);
}

struct Tuple(T...)
{
    T values;
    alias values this;
}
