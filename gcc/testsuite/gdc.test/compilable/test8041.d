// PERMUTE_ARGS:

struct Foo { }

void main()
{
    static Foo sf;  // ok
    __gshared Foo gf;  // was: Error: non-constant expression gf = 0
    __gshared int[1][1] arr;  // dup: Issue 6089
}
