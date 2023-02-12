/* { dg-do compile } */
/* { dg-options "-O" } */

static int vfork(); /* { dg-warning "used but never defined" } */
void f() { vfork(); }
