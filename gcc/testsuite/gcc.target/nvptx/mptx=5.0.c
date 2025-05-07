/* { dg-do assemble } */
/* { dg-options {-march=sm_30 -mptx=5.0} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { scan-assembler-times {(?n)^	\.version	5\.0$} 1 } } */
/* { dg-final { scan-assembler-times {(?n)^	\.target	sm_30$} 1 } } */

#if __PTX_ISA_VERSION_MAJOR__ != 5
#error wrong value for __PTX_ISA_VERSION_MAJOR__
#endif

#if __PTX_ISA_VERSION_MINOR__ != 0
#error wrong value for __PTX_ISA_VERSION_MINOR__
#endif

#if __PTX_SM__ != 300
#error wrong value for __PTX_SM__
#endif

int dummy;
