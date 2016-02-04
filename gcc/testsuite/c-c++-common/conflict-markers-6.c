/* Branch coverage of conflict marker detection:
   none of these should be reported as conflict markers.  */

int a0;

<< HEAD  /* { dg-error "expected" } */

int a1;

<<<< HEAD  /* { dg-error "expected" } */

int a2;

<<<<<< HEAD  /* { dg-error "expected" } */

int b0;

== HEAD  /* { dg-error "expected" } */

int b1;

==== HEAD  /* { dg-error "expected" } */

int b2;

====== HEAD  /* { dg-error "expected" } */

int c0;

>> HEAD  /* { dg-error "expected" } */

int c1;

>>>> HEAD  /* { dg-error "expected" } */

int c2;

>>>>>> HEAD  /* { dg-error "expected" } */
