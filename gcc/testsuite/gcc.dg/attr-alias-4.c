/* ICE on invalid alias attribute: PR 35434.  */
/* { dg-do compile } */
/* { dg-options "" } */
typedef int i __attribute__((alias("j"))); /* { dg-warning "ignored" } */
