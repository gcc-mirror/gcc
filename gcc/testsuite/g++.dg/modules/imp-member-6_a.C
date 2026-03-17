// PR c++/124478
// { dg-additional-options "-fmodules -fdump-lang-module" }
// { dg-module-cmi tools }

export module tools;
export template <typename T> struct myvector {
  T* t = nullptr;
  ~myvector() { delete t; }
};
export struct DedupeFilesPath {
    myvector<int> other;
};

// Lazy destructor
// { dg-final { scan-lang-dump-not {'::DedupeFilesPath@tools:.::__dt '} module } }
