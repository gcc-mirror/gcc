/* { dg-do compile } */
/* { dg-options "-misa=sm_53 -mptx=_" } */

#if __PTX_SM__ != 530
#error wrong value for __PTX_SM__
#endif
