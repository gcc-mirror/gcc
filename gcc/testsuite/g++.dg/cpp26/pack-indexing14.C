// PR c++/117937
// { dg-do compile { target c++26 } }

void operate_one(const int) {}

template<typename ...T>
void operate_multi(T... args)
{
    [&]<int idx>()
    {
       ::operate_one(args...[idx]);
    }.template operator()<0>();
}

int main()
{
    ::operate_multi(0);
}
