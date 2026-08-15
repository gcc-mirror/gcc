// PR ipa/123227
// { dg-lto-do run }
// { dg-lto-options {{-Os -flto -fipa-icf -fdump-ipa-icf-details}} }

// Link::get_vals and get_vals have identical bodies, but the member
// function may assume a non-null this while the free function must accept
// a null argument, so ICF must not unify them at WPA.  Both are called so
// that neither is removed before ICF runs, and both are noinline so that
// the calls survive as calls.

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
Link::get_vals()
{
    Val v = zero;
    for (Link* l = this; l; l = l->next)
        v |= l->val;
    return v;
}

extern Val get_vals(Link* l);

static Link one = { Val(5), 0 };

// Volatile so that the null argument is not propagated into get_vals.
Link *volatile nullp = 0;

int
main()
{
    if (one.get_vals() != Val(5))
        __builtin_abort();
    if (get_vals(nullp) != zero)
        __builtin_abort();
    return 0;
}

// { dg-final { scan-wpa-ipa-dump-not "Unified" "icf" } }
// { dg-final { scan-wpa-ipa-dump "Equal symbols: 0" "icf" } }
