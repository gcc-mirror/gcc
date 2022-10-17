struct Algebraic(T...)
{
    T t;
}

struct This;

struct While(T) { T[] body; }

alias Stmt = Algebraic!(While!(This));
