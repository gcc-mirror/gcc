/* PR 15236: pedantic switch modifies treatment of non-ISO compliant
   enumerations.  */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra -pedantic-errors -Wconversion" } */
typedef enum OMX_ERRORTYPE
{
  OMX_ErrorNone = 0,
  OMX_ErrorInsufficientResources = 0x80001000 /* { dg-error "ISO C restricts enumerator values to range of .int." } */
} OMX_ERRORTYPE;
