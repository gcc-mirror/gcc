/* { dg-do compile { target powerpc-*-eabi* } } */
/* { dg-options "-mcpu=8540 -mabi=spe -O0" } */

typedef int v2si __attribute__ ((vector_size (8)));

/* The two specializations must be considered different.  */
template <class T> class X		    { };
template <>        class X<__ev64_opaque__> { };
template <>        class X<v2si>	    { };
