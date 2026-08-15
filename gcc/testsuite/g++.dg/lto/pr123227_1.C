// PR ipa/123227

enum Val { zero = 0 };

inline Val&
operator|=(Val& a, Val b)
{
    return a = static_cast<Val>(static_cast<int>(a) | static_cast<int>(b));
}

struct Link {
    Val get_vals();
    Val val;
    Link* next;
};

Val __attribute__((noinline))
get_vals(Link* l)
{
    Val v = zero;
    for (; l; l = l->next)
        v |= l->val;
    return v;
}
