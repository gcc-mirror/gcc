/* Verify that debug information does not have unknown names for
   vector base types.  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-gdwarf-2 -maltivec" } */

__vector __bool vb;
__vector float vf;
__vector __pixel vp;
__vector signed char vsc;
__vector signed int vsi;
__vector signed short vss;
__vector unsigned char vuc;
__vector unsigned int vui;
__vector unsigned short vus;

/* { dg-final { scan-assembler-not "__unknown__" } } */
