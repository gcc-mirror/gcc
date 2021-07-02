/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

struct __attribute__((packed)) S /* { dg-warning "packed layout" } */
{
  char c;
  int  t;
};

/* { dg-final { cleanup-ada-spec } } */
