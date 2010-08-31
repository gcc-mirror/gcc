/* PR preprocessor/45457 */
/* { dg-do compile } */

const char *a =
#ifdef __DBL_DENORM_MIN__                                                                                                                         
"a"
#endif                                                                                                                                            
#if defined(__DBL_EPSILON__)
"b"
#endif
#ifndef __DBL_MAX__
"c"
#endif
#if !defined(__DBL_MIN__)
"d"
#endif
;
double b = __DBL_DENORM_MIN__ + __DBL_EPSILON__ + __DBL_MAX__ + __DBL_MIN__;
