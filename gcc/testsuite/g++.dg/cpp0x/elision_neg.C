// I, Howard Hinnant, hereby place this code in the public domain.

// Test: Implicit cast to rvalue when eliding copy

// { dg-do compile { target c++11 } }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {char x[1];};
struct two   {char x[2];};

class move_only
{
    move_only(const move_only&); // { dg-message "private" }
    move_only& operator=(const move_only&);
public:
    move_only() {}
    move_only(move_only&&) {}
    move_only& operator=(move_only&&) {return *this;}
};

move_only
test1()
{
    static move_only x;
    return x;  //  { dg-error "within this context" }
}

move_only
test2(move_only&& x)
{
    return x;  //  { dg-error "within this context" }
}

int main()
{
    move_only t1 = test1();
    move_only t2 = test2(move_only());
    return 0;
}

bool b = true;
