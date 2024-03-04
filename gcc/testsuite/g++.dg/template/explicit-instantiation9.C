// Fixits for specialisations are not valid for instantiations

template <typename T>
struct S {};

template struct S<int> {};  // { dg-error "explicit instantiation cannot have a definition" }
