/* Copyright 2000  Free Software Foundation */
/* by Alexandre Oliva  <aoliva@redhat.com> */

int
foo ()
{
  int bar; /* { dg-error "previous.*decl" "previous.*decl" } */
  volatile int bar; /* { dg-error "conflicting types" "conflicting types" } */
}

