// PR c++/71285
// { dg-options -std=c++1z }

template<typename... Args>
void spurious(Args... args)
{
    (... + args).member;
}

int main()
{
}
