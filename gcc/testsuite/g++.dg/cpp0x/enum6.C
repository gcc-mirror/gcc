// PR c++/37946
// { dg-options "-std=c++11" }

enum class E : char
{
    e1,
    e2
};

inline E operator| (E a1, E a2)
{
    char ret = static_cast<char> (a1)
        | static_cast<char> (a2);
    return static_cast<E>(ret);
}
