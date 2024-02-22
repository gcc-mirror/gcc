/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -Og" } */

/* PR target/81348: Compiler died in doing short->float conversion due to using
   the wrong register in a define_split.  */

int a;
short b;
float ***c;

void d(void)
{
        int e = 3;

        if (a)
                e = b;

        ***c = e;
}

/* { dg-final { scan-assembler {\mlha\M}  } } */
/* { dg-final { scan-assembler {\mmtvsrwa\M} } } */
