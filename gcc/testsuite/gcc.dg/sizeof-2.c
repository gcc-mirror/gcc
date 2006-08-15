/* { dg-do compile } */


void foo()
{
  sizeof(,); /* { dg-error "expected expression before" } */
}

