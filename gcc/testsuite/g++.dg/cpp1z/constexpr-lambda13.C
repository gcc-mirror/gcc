// { dg-do compile { target c++17 } }

auto l1 = []() constexpr constexpr { }; // { dg-error "duplicate" }
auto l2 = []() mutable mutable { }; // { dg-error "duplicate" }
auto l3 = []() static { };	    // { dg-error "static" }
