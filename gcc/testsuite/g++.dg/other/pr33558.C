/* { dg-do compile } */

class X {
  mutable int &q; /* { dg-error "3:reference .q. cannot be declared .mutable." } */
};
