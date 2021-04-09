// { dg-do compile { target c++20 } }

template <class T, class U>
concept Concept2 = requires (T t, U u) // { dg-error "depends on itself" }
{
    t += u;
};

template <class T>
concept Concept = Concept2 <T, T>;

struct S
{
    template <Concept T>
    constexpr S& operator += (T o);
};

constexpr S operator * (S a, S b)
{
    return a += b; // { dg-error "no match" }
}
