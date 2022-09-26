// PR c++/106567
// { dg-do compile { target c++11 } }

template <class V>
void urgh()
{
    const V x[] = {V(0), V(1), V(2), V(0)};

    [&]() {
        for (auto& v : x) {}
    }();
}

void no_urgh()
{
    using V = int;

    const V x[] = {V(0), V(1), V(2), V(0)};

    [&]() {
        for (auto& v : x) {}
    }();
}

int main()
{
    no_urgh();
    urgh<int>();
}
