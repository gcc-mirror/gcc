/* { dg-do compile } */
/* { dg-options "-misa=sm_35" } */

#if __PTX_SM__ != 350
#error wrong value for __PTX_SM__
#endif
