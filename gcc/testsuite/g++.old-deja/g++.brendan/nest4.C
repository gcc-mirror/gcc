// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed nested-classes
class vec {
    class blah { };

    ::vec::blah	satan( 0);// { dg-error "" } .*
    blah	herman( 0);// { dg-error "" } .*
};
