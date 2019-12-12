// PR c++/84374 - diagnose invalid uses of decltype(auto).
// { dg-do compile { target c++14 } }

auto l = [](auto* r)->decltype(auto)* { return r; }; // { dg-error "as type rather than plain" }
auto m = [](auto* r)->decltype(auto)& { return *r; }; // { dg-error "as type rather than plain" }

decltype(auto)* f(); // { dg-error "as type rather than plain" }
decltype(auto)& f2(); // { dg-error "as type rather than plain" }
decltype(auto)* f3() { return 42; } // { dg-error "as type rather than plain" }
decltype(auto)& f4() { return 42; } // { dg-error "as type rather than plain" }


class C {
  decltype(auto)* g(); // { dg-error "as type rather than plain" }
  decltype(auto)& g2(); // { dg-error "as type rather than plain" }
  decltype(auto)* g3() { } // { dg-error "as type rather than plain" }
  decltype(auto)& g4() { } // { dg-error "as type rather than plain" }
};
