// { dg-options -std=c++1z }

auto l1 = []() constexpr constexpr { }; // { dg-error "duplicate" }
auto l2 = []() mutable mutable { }; // { dg-error "duplicate" }
auto l3 = []() static { };	    // { dg-error "static" }
