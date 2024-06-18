/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

/* This used to ICE.  During gimplification, "i" is widened to an unsigned
   int.  We used to fail at expand time as we tried to cram an SImode item
   into a QImode memory slot.  This has been fixed to properly truncate the
   shift amount when splatting it into a vector.  */

typedef unsigned char v16ui __attribute__((vector_size(16)));

v16ui vslb(v16ui v, unsigned char i)
{
	return v << i;
}

/* { dg-final { scan-assembler "vspltb" } } */
/* { dg-final { scan-assembler "vslb" } } */
