// DR 2374
// { dg-do compile { target c++17 } }

enum class Orange;
enum class Apple;

extern Orange o;
Apple a{o}; // { dg-error "cannot convert" }
