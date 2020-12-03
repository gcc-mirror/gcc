/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef enum { LangC } cLanguage;
typedef enum { FunctionOneArg, FunctionStandard } cFunctionType;
void *CCTK_CallFunction_function;
cLanguage CCTK_CallFunction_fdata_0;
cFunctionType CCTK_CallFunction_fdata_1;
void CCTK_CallFunction_data() {
  void (*standardfunc)();
  int (*oneargfunc)();
  switch (CCTK_CallFunction_fdata_1) {
  case FunctionOneArg:
    oneargfunc = CCTK_CallFunction_function;
    oneargfunc(CCTK_CallFunction_data);
    break;
  case FunctionStandard:
    switch (CCTK_CallFunction_fdata_0) {
    case LangC:
      standardfunc = CCTK_CallFunction_function;
      standardfunc(CCTK_CallFunction_data);
    }
  }
}
