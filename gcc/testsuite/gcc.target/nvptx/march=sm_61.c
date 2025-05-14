/* { dg-do assemble } */
/* { dg-options {-march=sm_61 -mptx=_} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { scan-assembler-times {(?n)^	\.version	7\.3$} 1 } } */
/* { dg-final { scan-assembler-times {(?n)^	\.target	sm_61$} 1 } } */

#if __PTX_ISA_VERSION_MAJOR__ != 7
#error wrong value for __PTX_ISA_VERSION_MAJOR__
#endif

#if __PTX_ISA_VERSION_MINOR__ != 3
#error wrong value for __PTX_ISA_VERSION_MINOR__
#endif

#if __PTX_SM__ != 610
#error wrong value for __PTX_SM__
#endif

int dummy;
