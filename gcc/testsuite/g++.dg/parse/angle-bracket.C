// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// Origin: Bill Somerville <bill@classdesign.com>
// { dg-do compile }

int main()
{
  ( int() > int() );            // { dg-bogus "parse|syntax" "" { xfail *-*-* } }
  return 0;
}
