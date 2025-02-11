/* PR c/118838 */
/* { dg-do compile } */
/* { dg-additional-options "-Wunknown-pragmas" } */

/* Make sure the warnings are suppressed as expected.  */

/* The tokens need to be all on the same line here.  */
_Pragma ("GCC diagnostic push") _Pragma ("GCC diagnostic ignored \"-Wunknown-pragmas\"") _Pragma ("__unknown__") _Pragma ("GCC diagnostic pop")

#define MACRO \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wunknown-pragmas\"") \
    _Pragma ("__unknown__") \
    _Pragma ("GCC diagnostic pop")
MACRO
