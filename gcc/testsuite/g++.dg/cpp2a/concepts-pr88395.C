// { dg-do compile { target c++2a } }

template <class T, class U>
concept Concept2 = requires (T t, U u)
{
    t += u; // { dg-error "template instantiation depth" }
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
    return a += b;
}

// { dg-prune-output "compilation terminated" }
