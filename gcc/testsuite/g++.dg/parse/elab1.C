namespace Name {

    typedef void *(*Function)( void *, int ); // { dg-error "previous declaration" }

    struct Foo {
      struct Function xyz[5]; // { dg-error "" }
    };

}
