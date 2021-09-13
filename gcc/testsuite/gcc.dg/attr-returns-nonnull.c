/* Verify that attribute returns_nonnull on global and local function
   declarations is merged.
   { dg-do compile }
   { dg-options "-Wall -fdump-tree-optimized" } */

void foo (void);


void frnn_local_local (void)
{
  __attribute__ ((returns_nonnull)) void* frnn1 (void);

  if (!frnn1 ())
    foo ();
}

void gnr_local_local (void)
{
  void* frnn1 (void);

  if (!frnn1 ())
    foo ();
}

void frnn_local_global (void)
{
  __attribute__ ((returns_nonnull)) void* frnn2 (void);

  if (!frnn2 ())
    foo ();
}

void* frnn2 (void);

void gnr_local_global (void)
{
  if (!frnn2 ())
    foo ();
}

__attribute__ ((returns_nonnull)) void* frnn3 (void);

void frnn_global_local (void)
{
  if (!frnn3 ())
    foo ();
}

void gnr_global_local (void)
{
  void* frnn3 (void);

  if (!frnn3 ())
    foo ();
}


/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
