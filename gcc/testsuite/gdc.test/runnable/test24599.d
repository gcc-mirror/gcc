module mod;

struct Variable
{
    size_t toHash() const { return 0; }
}

enum hasInoutConstruction(T) = __traits(compiles, { struct S { T a; } });

struct Algebraic(T)
{
    static if (hasInoutConstruction!T)
    {
    }
}

Algebraic!Variable foo();

struct S
{
    Variable[] symbols;
}

void main() {}
