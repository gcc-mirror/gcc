// Build don't link:

  struct locale
  {
    template<class _Facet>
      locale (const locale&, _Facet*);
    locale(int*) throw();
  };
  void f(int* p) { locale keep (p); }
