// PR c++/71121
// { dg-do compile { target c++14 } }
// { dg-options -Waddress }

struct CC { void mbr(); };

constexpr auto getFunc() {
    return &CC::mbr;
}

constexpr bool xxx(void (CC::*_a)())
{
    constexpr auto f = getFunc();
    return (f == _a);
}
