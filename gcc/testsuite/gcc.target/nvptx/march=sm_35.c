/* { dg-do assemble } */
/* { dg-options {-march=sm_35 -mptx=_} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { scan-assembler-times {(?n)^	\.version	6\.0$} 1 } } */
/* { dg-final { scan-assembler-times {(?n)^	\.target	sm_35$} 1 } } */

#if __PTX_ISA_VERSION_MAJOR__ != 6
#error wrong value for __PTX_ISA_VERSION_MAJOR__
#endif

#if __PTX_ISA_VERSION_MINOR__ != 0
#error wrong value for __PTX_ISA_VERSION_MINOR__
#endif

#if __PTX_SM__ != 350
#error wrong value for __PTX_SM__
#endif

int dummy;
