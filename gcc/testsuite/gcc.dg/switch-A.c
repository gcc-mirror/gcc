/* { dg-do compile } */

void foo()
{
  switch (,) { } /* { dg-error "expected expression before" } */
}

