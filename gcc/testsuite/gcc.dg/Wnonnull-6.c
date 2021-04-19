/* Verify that attribute nonnull on global and local function declarations
   or those to pointers to functions is merged.
   { dg-do compile }
   { dg-options "-Wall" } */

void fnonnull_local_local (void)
{
  extern __attribute__ ((nonnull)) void fnonnull1 (void*);

  fnonnull1 (0);    // { dg-warning "\\\[-Wnonnull" }
}

void gnonnull_local_local (void)
{
  extern void fnonnull1 (void*);

  fnonnull1 (0);    // { dg-warning "\\\[-Wnonnull" }
}


void fnonnull_local_global (void)
{
  extern __attribute__ ((nonnull)) void fnonnull2 (void*);

  fnonnull2 (0);    // { dg-warning "\\\[-Wnonnull" }
}

extern void fnonnull2 (void*);

void gnonnull_local_global (void)
{
  fnonnull2 (0);    // { dg-warning "\\\[-Wnonnull" }
}


extern __attribute__ ((nonnull)) void fnonnull3 (void*);

void fnonnull_global_local (void)
{
  fnonnull3 (0);    // { dg-warning "\\\[-Wnonnull" }
}

void gnonnull_global_local (void)
{
  extern void fnonnull3 (void*);

  fnonnull3 (0);    // { dg-warning "\\\[-Wnonnull" }
}


void pfnonnull_local_local (void)
{
  extern __attribute__ ((nonnull)) void (*pfnonnull1) (void*);

  pfnonnull1 (0);   // { dg-warning "\\\[-Wnonnull" }
}

void gpnonnull_local_local (void)
{
  extern void (*pfnonnull1) (void*);

  pfnonnull1 (0);   // { dg-warning "\\\[-Wnonnull" "pr?????" { xfail *-*-* } }
}


void pfnonnull_local_global (void)
{
  extern __attribute__ ((nonnull)) void (*pfnonnull2) (void*);

  pfnonnull2 (0);   // { dg-warning "\\\[-Wnonnull" }
}

extern void (*pfnonnull2) (void*);

void gpnonnull_local_global (void)
{
  pfnonnull2 (0);   // { dg-warning "\\\[-Wnonnull" "pr?????" { xfail *-*-* } }
}


extern __attribute__ ((nonnull)) void (*pfnonnull3) (void*);

void pfnonnull_global_local (void)
{
  pfnonnull3 (0);   // { dg-warning "\\\[-Wnonnull" }
}

void gpnonnull_global_local (void)
{
  extern void (*pfnonnull3) (void*);

  pfnonnull3 (0);   // { dg-warning "\\\[-Wnonnull" }
}
