/* PR middle-end/101751 - attribute access none with void pointer expects
   nonzero size
   { dg-do compile }
   { dg-options "-Wall" } */

__attribute__ ((access (none, 1))) void
fvp_m1 (const void*);

void nowarn_m1 (void)
{
  /* Verify these don't trigger a warning for calls to a function
     declared with attribute access none.  */
  fvp_m1 ((void*)-1);         // { dg-bogus "-Wstringop-" }
  fvp_m1 ((void*)1);          // { dg-bogus "-Wstringop-" }
}


__attribute__ ((access (none, 1))) void
fvp_none (void*);

void nowarn_c_cp1 (void)
{
  char c;
  fvp_none (&c);
  fvp_none (&c + 1);          // { dg-bogus "-Wstringop-" }
}

void nowarn_f_fp1 (void)
{
  fvp_none ((char*)&nowarn_f_fp1);
  fvp_none ((char*)&nowarn_f_fp1 + 1);
}

void nowarn_sp1_sp_4 (void)
{
  fvp_none ("" + 1);          // { dg-bogus "-Wstringop-" }
  fvp_none ("123" + 4);       // { dg-bogus "-Wstringop-" }
}


__attribute__ ((access (none, 1))) void
wfvp_none (void*);            // { dg-message "in a call to function 'wfvp_none' declared with attribute 'access \\\(none, 1\\\)'" }

void warn_cm1_p1 (void)
{
  char c;
  /* With optimization both of the following are diagnosed by -Warray-bounds.
     The second also without optimization by -Wstringop-overread.  They
     should both be diagnosed by the same warning even without optimization. */
  wfvp_none (&c - 1);         // { dg-warning "" "pr??????" { xfail *-*-* } }
  wfvp_none (&c + 2);         // { dg-warning "" }
}

void warn_fp2 (void)
{
  void *p = (char*)&warn_fp2 + sizeof warn_fp2;
  fvp_none (p);               // { dg-warning "" "pr??????" { xfail *-*-* } }
}

void warn_sp2 (void)
{
  wfvp_none ("" + 2);         // { dg-warning "" }
}
