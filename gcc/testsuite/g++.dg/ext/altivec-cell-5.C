/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Basic test for the new VMX intrinsics and error messages.  */
#include <altivec.h>

int main(int argc, char **argv)
{
vector float t;
    vec_promote();                      /* { dg-error "vec_promote only accepts 2" } */
    vec_promote(1.0f);                  /* { dg-error "vec_promote only accepts 2" } */
    vec_promote(1.0f, 2, 3);            /* { dg-error "vec_promote only accepts 2" } */
    vec_extract ();                     /* { dg-error "vec_extract only accepts 2" } */
    vec_extract (t);                    /* { dg-error "vec_extract only accepts 2" } */
    vec_extract (t, 2);
    vec_extract (t, 2, 5, 6);           /* { dg-error "vec_extract only accepts 2" } */
    vec_splats ();                      /* { dg-error "vec_splats only accepts 1" } */
    vec_splats (t, 3);                  /* { dg-error "vec_splats only accepts 1" } */
    vec_insert ();                      /* { dg-error "vec_insert only accepts 3" } */
    vec_insert (t);                     /* { dg-error "vec_insert only accepts 3" } */
    vec_insert (t, 3);                  /* { dg-error "vec_insert only accepts 3" } */
    vec_insert (t, 3, 2, 4, 6, 6);      /* { dg-error "vec_insert only accepts 3" } */
    return 0;
}  
