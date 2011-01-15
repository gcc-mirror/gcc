/* { dg-do compile } */

class X {
  mutable int &q; /* { dg-error "cannot be declared 'mutable'" } */
};
