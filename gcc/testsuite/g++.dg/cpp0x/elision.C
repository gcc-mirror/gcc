// I, Howard Hinnant, hereby place this code in the public domain.

// Test: Implicit cast to rvalue when eliding copy

// { dg-do compile }
// { dg-options "-std=c++0x" }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {char x[1];};
struct two   {char x[2];};

class move_only
{
    move_only(const move_only&);
    move_only& operator=(const move_only&);
public:
    move_only() {}
    move_only(move_only&&) {}
    move_only& operator=(move_only&&) {return *this;}
};

move_only
test1()
{
    return move_only();
}

move_only
test2()
{
    move_only x;
    return x;
}

move_only
test3(bool b)
{
    move_only x1;
    if (b)
    {
        move_only x2;
        return x2;
    }
    return x1;
}

void
test4(bool b)
{
    if (!b)
        throw move_only();
}

void
test5(bool b)
{
    move_only x;
    if (!b)
        throw x;
}

extern bool b;

int main()
{
    move_only t1 = test1();
    move_only t2 = test2();
    move_only t3 = test3(b);
    test4(b);
    test5(b);
    return 0;
}

bool b = true;
