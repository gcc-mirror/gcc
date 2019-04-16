// { dg-additional-options "-std=c++14 -fconcepts" }

a(decltype((0 > 1e91 && 1e31 && (auto))));  // { dg-error "expected" }
