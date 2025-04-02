/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("test"))) float
foo () { return 3; } /* { dg-error "invalid feature modifier .test. of value .test. in .target_version. attribute" } */

__attribute__ ((target_version ("sve+test"))) float
foo2 () { return 3; } /* { dg-error "invalid feature modifier .test. of value .sve.test. in .target_version. attribute" } */
