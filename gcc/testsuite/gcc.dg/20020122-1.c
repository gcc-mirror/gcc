/* Copyright (C) 2002  Free Software Foundation.
   by Hans-Peter Nilsson  <hp@axis.com> */

/* { dg-do compile } */

typedef struct
 {
   unsigned short s;
 } t;

struct u
 {
   t v;
 };

int
foo (struct u *uu)
{
  int i = ((unsigned int) ((uu->v) & 42)); /* { dg-error "invalid operands to binary" } */
  return i;
}
