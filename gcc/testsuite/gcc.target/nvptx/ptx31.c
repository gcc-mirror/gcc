/* { dg-do compile } */
/* { dg-options "-march=sm_30 -mptx=3.1" } */

#if __PTX_ISA_VERSION_MAJOR__ != 3
#error wrong value for __PTX_ISA_VERSION_MAJOR__
#endif

#if __PTX_ISA_VERSION_MINOR__ != 1
#error wrong value for __PTX_ISA_VERSION_MINOR__
#endif
