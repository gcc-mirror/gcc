/* { dg-do compile } */
/* { dg-options "-misa=sm_70 -mptx=_" } */

#if __PTX_SM__ != 700
#error wrong value for __PTX_SM__
#endif
