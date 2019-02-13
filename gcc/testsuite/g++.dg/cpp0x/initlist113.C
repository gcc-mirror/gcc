// PR c++/89297
// { dg-do compile { target c++11 } }

int id(int v) { return v; }
float id(float v) { return v; }

template <typename>
int foo(int v)
{
    return int{id(v)};
}
