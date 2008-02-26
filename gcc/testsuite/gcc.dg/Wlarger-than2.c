/* { dg-do compile } */
/* { dg-options "-Wlarger-than=8" } */
static void foo (void) 
{
  char buf[9]; /* { dg-warning "size of.*9 bytes" } */
}

