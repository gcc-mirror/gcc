/* { dg-do compile } */
/* { dg-options "-misa=sm_53" } */

#if __PTX_SM__ != 530
#error wrong value for __PTX_SM__
#endif
