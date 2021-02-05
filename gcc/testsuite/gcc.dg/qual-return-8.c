/* Same as qual-return-7.c but in C11 mode.
   { dg-do compile }
   { dg-options "-std=gnu11" } */

void test_local (void)
{
#if 0
  /* _Atomic is not considered a qualifier and so is not ignored
     on a return type.  As a result, the redeclaration below isn't
     valid.  See also qual-return-5.c.  */
  auto int fi_ai ();
  _Atomic int fi_ai () { return 0; }
#endif

  auto int fi_ci ();
  const int fi_ci () { return 0; }

  auto enum E fe_ce ();

  enum E { e };
  const enum E fe_ce () { return 0; }

  auto void fv_vv (void);
  volatile void fv_vv () { }

  auto volatile void fvv_v (void);
  void fvv_v () { }
}
