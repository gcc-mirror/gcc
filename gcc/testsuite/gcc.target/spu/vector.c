/* { dg-do compile } */
/* { dg-options "" } */

#ifndef __VECTOR_KEYWORD_SUPPORTED__
#error __VECTOR_KEYWORD_SUPPORTED__ is not defined
#endif

/* __vector is expanded unconditionally.  */
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

/* vector is expanded conditionally, based on the context.  */
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
