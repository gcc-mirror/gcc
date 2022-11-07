// Verify __has_attribute(init_priority) is false on targets that
// don't support init priorities, and is treated as an unrecognized
// attribute in that case.

#if !__has_attribute(init_priority)
#error unsupported // { dg-error "" "" { target { ! init_priority } } }
#endif

struct A { A(); } a __attribute__((init_priority(500)));
// { dg-warning "attribute directive ignored" "" { target { ! init_priority } } .-1 }
