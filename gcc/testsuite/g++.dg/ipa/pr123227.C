// PR ipa/123227
// { dg-do run }
// { dg-options "-Os -fipa-icf -fdump-ipa-icf-details" }

// Link::get_vals and get_vals have identical bodies, but the member
// function may assume a non-null this while the free function must accept
// a null argument, so ICF must not unify them.

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

Val
Link::get_vals()
{
    Val v = zero;
    for (Link* l = this; l; l = l->next)
        v |= l->val;
    return v;
}

Val
get_vals(Link* l)
{
    Val v = zero;
    for (; l; l = l->next)
        v |= l->val;
    return v;
}

int
main(int, char**)
{
    if (get_vals(0) != zero)
        __builtin_abort();
    return 0;
}

// { dg-final { scan-ipa-dump-not "Unified" "icf" } }
// { dg-final { scan-ipa-dump "Equal symbols: 0" "icf" } }
