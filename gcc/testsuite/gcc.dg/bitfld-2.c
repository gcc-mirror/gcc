/* Copyright (C) 2002 Free Software Foundation, Inc.

   Tests we warn about overly-large assignments to bitfields.

   Source: Neil Booth, 28 Jan 2002.
*/

struct bf
{
  unsigned int a: 2;
  int b: 2;
};

struct bf p = {4, 0};		/* { dg-warning "unsigned conversion from .int. to 'unsigned char:2' changes value from .4. to .0." } */
struct bf q = {0, 2};		/* { dg-warning "overflow in conversion from .int. to .signed char:2. changes value from .2. to .-2." } */
struct bf r = {3, -2};		/* { dg-bogus "(trunc|overflow)" } */

void foo ()
{
  p.a = 4, p.b = 0;		/* { dg-warning "unsigned conversion from .int. to .unsigned char:2. changes value from .4. to .0." } */
  q.a = 0, q.b = 2;		/* { dg-warning "overflow in conversion from .int. to .signed char:2. changes value from .2. to .-2." } */
  r.a = 3, r.b = -2;		/* { dg-bogus "(trunc|overflow)" } */
}
