// { dg-do compile }
// Copyright (C) 2001, 2003 Free Software Foundation, Inc.
// Contributed by Ben Elliston <bje@redhat.com>

// PR 80: Packed enums use minimum required storage.

extern "C" void abort();

enum numbers { one, two, three } __attribute__ ((packed)) nums; // { dg-bogus "" "" { xfail *-*-* } } 
enum colours { red = 1000, green, blue } __attribute__ ((packed)) cols; // { dg-bogus "" "" { xfail *-*-* } } 
enum conditions { fine, rain, cloudy } __attribute__ ((packed)) forecast; // { dg-bogus "" "" { xfail *-*-* } } 

int
main()
{
  if (sizeof (nums) != 1)
    abort ();

  if (sizeof (cols) != 2)
    abort ();

  if (sizeof (forecast) != 1)
    abort ();

  return 0;
}
