/* { dg-do compile } */
/* { dg-options "-ansi" } */

/* This is done by spu_internals.h, but we not include it here to keep
   down the dependencies.  */

#ifndef __VECTOR_KEYWORD_SUPPORTED__
#define vector __vector
#endif

/* __vector is expanded unconditionally by the preprocessor.  */
__vector int vi;
__vector unsigned char vuc;
__vector signed char vsc;
__vector unsigned short vus;
__vector signed short vss;
__vector unsigned int vui;
__vector signed int vsi;
__vector unsigned long long ull;
__vector signed long long sll;
__vector float vf;
__vector double vd;

/* vector is expanded by the define above, regardless of context.  */
vector int vi;
vector unsigned char vuc;
vector signed char vsc;
vector unsigned short vus;
vector signed short vss;
vector unsigned int vui;
vector signed int vsi;
vector unsigned long long ull;
vector signed long long sll;
vector float vf;
vector double vd;
