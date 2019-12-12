/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

class X {
  mutable int &q; /* { dg-warning "3:reference .q. cannot be declared .mutable." } */
};
