// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 2004 <nathan@codesourcery.com>

// PR 18905. bogus error
// Origin:  Andrew Pinski  <pinskia@gcc.gnu.org>

int f1(char);
template <int t>
void f(void)
{
 const char* const suffixes = "plpv";
 f1(suffixes[t]);
}
