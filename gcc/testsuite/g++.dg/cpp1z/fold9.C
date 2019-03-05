// PR c++/71285
// { dg-do compile { target c++17 } }

template<typename... Args>
void spurious(Args... args)
{
    (... + args).member;
}

int main()
{
}
