/* Copyright (C) 2002 Free Software Foundation, Inc.

   Tests we warn about overly-large assignments to bitfields.

   Source: Neil Booth, 28 Jan 2002.
*/

struct bf
{
  unsigned int a: 2;
  int b: 2;
};

struct bf p = {4, 0};		/* { dg-warning "truncated" "" { xfail *-*-* } } */
struct bf q = {0, 2};		/* { dg-warning "overflow" "" { xfail *-*-* } } */
struct bf r = {3, -2};		/* { dg-bogus "(truncated|overflow)" } */

void foo ()
{
  p.a = 4, p.b = 0;		/* { dg-warning "truncated" "" { xfail *-*-* } } */
  q.a = 0, q.b = 2;		/* { dg-warning "overflow" "" { xfail *-*-* } } */
  r.a = 3, r.b = -2;		/* { dg-bogus "(truncated|overflow)" } */
}
