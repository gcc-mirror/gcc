namespace Name {

    typedef void *(*Function)( void *, int );

    struct Foo {
      struct Function xyz[5]; // { dg-error "" }
    };

}
