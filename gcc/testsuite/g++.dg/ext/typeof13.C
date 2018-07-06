// PR c++/84701
// { dg-options "-pedantic" }

typedef short foo_t;
foo_t s = -1;  /* FFFF */

unsigned u = (unsigned foo_t)s;	       // { dg-warning foo_t }
unsigned u2 = (unsigned __typeof(s))s;   // { dg-error typeof }
