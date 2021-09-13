/* Verify that attribute noreturn on global and local function declarations
   is merged.
   { dg-do compile }
   { dg-options "-Wall" } */

int fnr_local_local (void)
{
  __attribute__ ((noreturn)) void fnr1 (void);

  fnr1 ();
  // no return, no warning (good)
}

int gnr_local_local (void)
{
  void fnr1 (void);

  fnr1 ();
  // no return, no warning (good)
}


int fnr_local_global (void)
{
  __attribute__ ((noreturn)) void fnr2 (void);

  fnr2 ();
  // no return, no warning (good)
}

void fnr2 (void);

int gnr_local_global (void)
{
  fnr2 ();
  // no return, no warning (good)
}


__attribute__ ((noreturn)) void fnr3 (void);

int fnr_global_local (void)
{
  fnr3 ();
  // no return, no warning (good)
}

int gnr_global_local (void)
{
  void fnr3 (void);

  fnr3 ();
  // no return, no warning (good)
}
