/* { dg-do compile } */
/* { dg-options "-mcpu=8540 -mspe -mabi=spe -mfloat-gprs=single -O0" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } } */

typedef int v2si __attribute__ ((vector_size (8)));

/* The two specializations must be considered different.  */
template <class T> class X		    { };
template <>        class X<__ev64_opaque__> { };
template <>        class X<v2si>	    { };
