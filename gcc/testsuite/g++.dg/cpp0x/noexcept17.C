// PR c++/50043
// { dg-options -std=c++11 }

struct True1 {};
struct True2 { ~True2(); };
struct True3 { ~True3(){ throw 0; } };
struct False { ~False() noexcept(false); };

template <typename Base>
struct A : Base
{
};

template <typename Member>
struct B
{
    Member mem;
};

template <typename Base, typename Member>
struct C : Base
{
    Member mem;
};

#define SA(X) static_assert(X, #X)

SA( noexcept(True1()));
SA( noexcept(True2()));
SA( noexcept(True3()));
SA(!noexcept(False()));

SA( noexcept(A<True1>()));
SA( noexcept(A<True2>()));
SA( noexcept(A<True3>()));
SA(!noexcept(A<False>()));

SA( noexcept(B<True1>()));
SA( noexcept(B<True2>()));
SA( noexcept(B<True3>()));
SA(!noexcept(B<False>()));

SA( noexcept(C<True1, True2>()));
SA( noexcept(C<True1, True3>()));
SA( noexcept(C<True2, True3>()));
SA( noexcept(C<True2, True1>()));
SA( noexcept(C<True3, True1>()));
SA( noexcept(C<True3, True2>()));
SA(!noexcept(C<False, True1>()));
SA(!noexcept(C<False, True2>()));
SA(!noexcept(C<False, True3>()));
SA(!noexcept(C<True1, False>()));
SA(!noexcept(C<True2, False>()));
SA(!noexcept(C<True3, False>()));
