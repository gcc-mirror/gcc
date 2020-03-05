// PR c++/92531
// { dg-do compile { target c++17 } }

template <typename XK>
void ky () noexcept ([]{}); // IFNDR
// Optional error: void(*)() to bool conv in converted constant expression
// { dg-prune-output "converted constant expression" }
