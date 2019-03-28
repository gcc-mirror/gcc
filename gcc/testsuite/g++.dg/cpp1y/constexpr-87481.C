// PR c++/87481
// { dg-do compile { target c++14 } }
// { dg-options "-fconstexpr-loop-limit=98304 -fconstexpr-ops-limit=131072" } */

constexpr unsigned
foo ()
{
  unsigned int r = 0;
  for (int i = 0; i < 65536; i++)
    for (int j = 0; j < 65536; j++)
      for (int k = 0; k < 65536; k++)	// { dg-error "'constexpr' evaluation operation count exceeds limit of 131072" "" { target *-*-* } 0 }
	r += (i + j + k);
  return r;
}

constexpr auto x = foo ();		// { dg-message "in 'constexpr' expansion of" }
