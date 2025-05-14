// PR c++/114630
// { dg-additional-options "-fmodules-ts -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;
template <typename> struct allocator {
  allocator() {}
};
export module M;
template class allocator<wchar_t>;

// allocator is reachable because the explicit instantiation is in purview.
// { dg-final { scan-lang-dump {Wrote declaration[^\n]*allocator} module } }
