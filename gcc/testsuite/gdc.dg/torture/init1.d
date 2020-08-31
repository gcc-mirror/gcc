// { dg-do run }
// { dg-options "-fno-druntime" }
// 'a' should not be default initialized to -1.
static char a = void;

extern (C) void main()
{
    assert(a == 0);
}
