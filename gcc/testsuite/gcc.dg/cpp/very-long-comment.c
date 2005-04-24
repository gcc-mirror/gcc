/*
This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment. This is a really long comment.
 */
/* This testcase for PR 20907 is a bit finicky about the placement of
   comment so avoid editing the text above here. */
/* { dg-options "-Wall" } */
/* { dg-do compile } */
#warning test warning /* { dg-warning "test warning" } */
#include <stdio.h>
int main()
{
  printf("This is line %d\n", __LINE__);
  return 0;
}
