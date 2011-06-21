/* { dg-do compile } */

extern __attribute__ ((always_inline)) void
 bar() { } /* { dg-warning "function might not be inlinable" } */

void
f()
{
  bar(); 
}

