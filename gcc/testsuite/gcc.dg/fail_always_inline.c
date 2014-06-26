/* { dg-do compile } */
/* { dg-add-options bind_pic_locally } */

extern __attribute__ ((always_inline)) void
 bar() { } /* { dg-warning "function might not be inlinable" } */

void
f()
{
  bar(); 
}

