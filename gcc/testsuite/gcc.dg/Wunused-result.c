/* PR c/99972 - missing -Wunused-result on a call to a locally redeclared
   warn_unused_result function
   { dg-do compile }
   { dg-options "-Wall" } */

void gwur_local_local (void)
{
  __attribute__ ((warn_unused_result)) int fwur1 (void);

  fwur1 ();         // { dg-warning "\\\[-Wunused-result" }
}

void hwur_local_local (void)
{
  /* Verify the attribute from the declaration above is copied/merged
     into the declaration below.  */
  int fwur1 (void);

  fwur1 ();          // { dg-warning "\\\[-Wunused-result" }
}


void gwur_local_global (void)
{
  __attribute__ ((warn_unused_result)) int fwur2 (void);

  fwur2 ();         // { dg-warning "\\\[-Wunused-result" }
}

int fwur2 (void);

void hwur_local_global (void)
{
  fwur2 ();         // { dg-warning "\\\[-Wunused-result" }
}


__attribute__ ((warn_unused_result)) int fwur3 (void);

void gwur_global_local (void)
{
  fwur3 ();         // { dg-warning "\\\[-Wunused-result" }
}

void hwur_global_local (void)
{
  int fwur3 (void);

  fwur3 ();         // { dg-warning "\\\[-Wunused-result" }
}
