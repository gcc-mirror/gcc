// { dg-options -std=c++11 }

enum Unscoped { };
enum class Scoped { };

Unscoped bar(Unscoped x) { return x; }
Scoped bar(Scoped x) { return x; }

auto var1u = bar(Unscoped()); // OK
auto var1s = bar(Scoped()); // OK

auto var2u = bar(Unscoped{}); // #1 Error, but should work
auto var2s = bar(Scoped{}); // #2 Error, but should work
