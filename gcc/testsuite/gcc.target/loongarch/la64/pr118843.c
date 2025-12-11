/* { dg-do preprocess } */
/* { dg-options "-mfrecipe -mfpu=none" } */

#ifdef __loongarch_frecipe
#error __loongarch_frecipe should not be avaliable here
#endif
