/* { dg-do compile } */
/* { dg-options "-O2 -mv850e3v5" } */

double frob (double r)
{
    r = -r;
    return r;
}
