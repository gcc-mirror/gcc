/* { dg-do compile } */
/* { dg-options "-misa=sm_80" } */

#if __PTX_SM__ != 800
#error wrong value for __PTX_SM__
#endif
