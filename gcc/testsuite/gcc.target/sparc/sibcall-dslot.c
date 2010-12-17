/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int one ();

int some_n ()
{
    return one ();
}

/* { dg-final { scan-assembler-not "nop" } } */
