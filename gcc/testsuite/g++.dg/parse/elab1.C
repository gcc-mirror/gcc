namespace Name {

    typedef void *(*Function)( void *, int ); // { dg-message "previous declaration" }

    struct Foo {
      struct Function xyz[5]; // { dg-error "" }
    };

}
