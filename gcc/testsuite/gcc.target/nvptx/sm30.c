/* { dg-do compile } */
/* { dg-options "-misa=sm_30" } */

#if __PTX_SM__ != 300
#error wrong value for __PTX_SM__
#endif
