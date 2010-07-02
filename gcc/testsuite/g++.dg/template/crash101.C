// PR c++/44039

struct locale {  };

template<class charT>
  void
  foo()
  { locale::locale(); } // { dg-error "cannot call|function-style" }

void
bar()
{ foo<char>(); }
