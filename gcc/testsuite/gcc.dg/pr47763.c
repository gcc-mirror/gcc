/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -fdump-rtl-web" } */

int
foo()
{
}

/* { dg-final { scan-rtl-dump-not "Web oldreg" "web" } } */
