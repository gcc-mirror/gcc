/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -Og" } */

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

/* { dg-final { scan-assembler {\mlxsihzx\M}  } } */
/* { dg-final { scan-assembler {\mvextsh2d\M} } } */
