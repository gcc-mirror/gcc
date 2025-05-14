enum Foo<'a> {}

enum Bar<'a> {
    in_band_def_explicit_impl(Foo<'a>),
}
