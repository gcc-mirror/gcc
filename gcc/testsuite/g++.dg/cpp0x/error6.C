// PR c++/48284
// { dg-options -std=c++0x }

template<typename C>
auto g(C& c) -> decltype (c.f()) { return c.f(); } // { dg-message "decltype .c\\.f" }

template<typename C>
auto g(C& c) -> decltype (c.f()) { return c.f(); } // { dg-error "decltype .c\\.f" }
