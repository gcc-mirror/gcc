// PR c++/98717
// { dg-do compile { target c++20 } }

template<typename... T>
concept True = true;

static_assert(True<>);
