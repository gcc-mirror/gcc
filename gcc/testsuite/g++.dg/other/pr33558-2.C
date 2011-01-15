/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

class X {
  mutable int &q; /* { dg-warning "cannot be declared 'mutable'" } */
};
