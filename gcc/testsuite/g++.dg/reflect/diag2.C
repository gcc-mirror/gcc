// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// This used to say ''splice_scope' not supported by
// dump_type_prefix<typeprefixerror>r'splice_scope' not supported
// by dump_type_suffix'.  Eeech.

using info = decltype(^^void);

struct S {
  int i;
};

template<info R>
void
f ()
{
  typename [:R:] r{1};  // { dg-message "previous declaration as .\\\[: R :\\\] r." }
  typename [:R:] r{2};  // { dg-error "conflicting declaration .\\\[: R :\\\] r." }
}

int
main ()
{
  f<^^S>();
}
