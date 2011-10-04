// PR c++/50442

template <typename T> struct MoveRef { operator T& () {} };
template <typename T> MoveRef <T> Move(T&) {}
struct Thing {};
Thing foo(const Thing* p) { return Thing(Move(*p)); }
