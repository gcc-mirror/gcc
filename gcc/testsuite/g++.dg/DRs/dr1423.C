// DR 1423 - Convertibility of nullptr to bool.
// { dg-do compile { target c++11 } }

bool b = nullptr; // { dg-error "converting to .bool. from .std::nullptr_t. requires direct-initialization" }
bool b2(nullptr);
bool b3{nullptr};
bool b4 = { nullptr }; // { dg-error "converting to .bool. from .std::nullptr_t. requires direct-initialization" }
