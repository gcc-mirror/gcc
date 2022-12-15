/* Regression test for ICE.  */
/* { dg-additional-options "-Wno-analyzer-possible-null-argument" } */
/* { dg-additional-options "-Wno-analyzer-malloc-leak" } */
/* { dg-additional-options "-Wno-analyzer-possible-null-dereference" } */

/* { dg-additional-options "-O1 --param analyzer-max-svalue-depth=5" } */

struct locale {
  class _Impl;
  _Impl *_M_impl;

  template <typename _Facet>
  locale (const locale &, _Facet *);

  static locale
  classic ();
};

struct locale::_Impl {
  _Impl (_Impl, int);
};

template <typename _Facet>
locale::locale (const locale &, _Facet *)
{
  new _Impl (*_M_impl, 1);
}

struct codecvt {
  virtual void do_max_lengththrow ();
};

void
test01 ()
{
  locale (locale::classic (), new codecvt);
}
