/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-mthumb -fpic -mpic-register=9" } */

int g_test;

int
foo (int par)
{
    g_test = par;
}
