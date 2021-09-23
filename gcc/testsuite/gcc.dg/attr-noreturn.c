/* Verify that attribute noreturn on global and local function declarations
   is merged.
   { dg-do compile }
   { dg-options "-Wall -fdump-tree-optimized" } */

void foo (void);

int fnr_local_local (void)
{
  __attribute__ ((noreturn)) void fnr1 (void);

  fnr1 ();

  foo ();
}

int gnr_local_local (void)
{
  void fnr1 (void);

  fnr1 ();

  foo ();
}


int fnr_local_global (void)
{
  __attribute__ ((noreturn)) void fnr2 (void);

  fnr2 ();

  foo ();
}

void fnr2 (void);

int gnr_local_global (void)
{
  fnr2 ();

  foo ();
}


__attribute__ ((noreturn)) void fnr3 (void);

int fnr_global_local (void)
{
  fnr3 ();

  foo ();
}

int gnr_global_local (void)
{
  void fnr3 (void);

  fnr3 ();

  foo ();
}

/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
