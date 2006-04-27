/* Darwin pragma for __attribute__ ((ms_struct)).  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-Wall" } */

#pragma ms_struct on

#pragma ms_struct off

#pragma ms_struct reset

#pragma ms_struct /* { dg-warning "malformed" } */

#pragma ms_struct on top of spaghetti /* { dg-warning "junk" } */

struct foo
{
  int a;
  int b;
  char c;
};
