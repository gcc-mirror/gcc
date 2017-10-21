/* { dg-do compile } */
/* { dg-options "-fcf-protection=none -mno-cet" } */

int var1 __attribute__((nocf_check)); /* { dg-warning "'nocf_check' attribute only applies to function types" } */
int *var2 __attribute__((nocf_check)); /* { dg-warning "'nocf_check' attribute only applies to function types" } */
void (**var3) (void) __attribute__((nocf_check)); /* { dg-warning "'nocf_check' attribute only applies to function types" } */
