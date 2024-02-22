/* { dg-do assemble { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O3" } */

/* Compile with -O3 -mcpu=power9.  It originally generated

        stxsd 12,1(9)

   which is illegal.  */

#pragma pack(1)
struct {
        signed : 1;
        unsigned long a;
} b;

void c(void)
{
        b.a = 0;
        for (; b.a <= 45; b.a = (long)b.a + 1)
                ;
}
