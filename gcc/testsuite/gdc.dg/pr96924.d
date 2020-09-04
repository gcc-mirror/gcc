// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96924
// { dg-do compile }

struct Memo
{
    string source;
    this(this);
}

void compile(string src, size_t end)
{
    Memo[] stack;
    stack  ~= Memo(src[end .. $]);
}
