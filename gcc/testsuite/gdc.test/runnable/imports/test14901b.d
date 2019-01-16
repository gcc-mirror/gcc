module imports.test14901b;

import imports.test14901a;

alias bar = make!"bar";

struct User(int id)
{
    int foo()
    {
        return bar;
    }
}
