/* Missing option arguments take precedence over wrong-language
   warnings.  */
/* { dg-do compile } */
/* { dg-options "-ftemplate-depth=" } */

/* { dg-error "missing argument" "" { target *-*-* } 0 } */
