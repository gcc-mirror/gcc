// PR c++/50248, DR 1358
// { dg-do compile { target c++11 } }

template<class Elt, unsigned max>
struct earray
{
    Elt elts[max];
    earray() = default;
    template<typename... Elt2>
    constexpr earray(Elt2&& ... e): elts(0) { }
};

struct SessionData
{
    SessionData(SessionData&) = delete;
    SessionData() = default;
};

struct MapSessionData : SessionData
{
    earray<short, 11> equip_index;
};

void test()
{
    MapSessionData *sd = new MapSessionData;
}
