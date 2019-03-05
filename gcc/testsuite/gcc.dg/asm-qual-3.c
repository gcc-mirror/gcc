/* Test that asm-qualifiers are not allowed on toplevel asm.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

asm const ("");    /* { dg-error {expected '\(' before 'const'} } */
asm volatile (""); /* { dg-error {expected '\(' before 'volatile'} } */
asm restrict (""); /* { dg-error {expected '\(' before 'restrict'} } */
asm inline ("");   /* { dg-error {expected '\(' before 'inline'} } */
asm goto ("");     /* { dg-error {expected '\(' before 'goto'} } */
