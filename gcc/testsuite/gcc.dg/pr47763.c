/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -fdump-rtl-web" } */

foo()
{
}

/* { dg-final { scan-rtl-dump-not "Web oldreg" "web" } } */
/* { dg-final { cleanup-rtl-dump "web" } } */
