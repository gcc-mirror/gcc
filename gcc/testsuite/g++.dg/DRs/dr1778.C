// P1286R2: Contra CWG1778
// { dg-do compile { target c++11 } }

struct T { T(); T(T &&) noexcept(false); };
struct U { T t; U(); U(U &&) noexcept = default; };
U u1;
U u2 = static_cast<U&&>(u1);      // OK, calls std::terminate if T::T(T&&) throws
