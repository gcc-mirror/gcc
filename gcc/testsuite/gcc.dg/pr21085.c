/* { dg-do compile } */
/* This used to cause excessive use, or a stack overflow, depending on which
   came first.  */
void foo (void)
{
  int maxstringlen = 1;
  int limit = 0, maxblock = 0, maxblockrem = 0;
  maxblockrem = (maxstringlen) % (2147483647 + 1); /* { dg-warning "overflow" } */
}
