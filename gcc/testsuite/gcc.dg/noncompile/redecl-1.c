/* Copyright 2000  Free Software Foundation */
/* by Alexandre Oliva  <aoliva@redhat.com> */

int
foo ()
{
  int bar; /* { dg-error "previous declaration" "previously declared" } */
  volatile int bar; /* { dg-error "redeclaration" "redeclaration" } */
}

