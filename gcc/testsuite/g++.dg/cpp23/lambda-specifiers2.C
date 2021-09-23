// PR c++/99850
// P1102R2 - Down with ()!
// { dg-do compile { target c++23 } }

auto l = []<auto> requires true -> void {};
template <typename...> concept C = true;
auto m = []<typename... Ts> requires (C<Ts> && ...) -> void {};
