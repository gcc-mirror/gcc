/* PR middle-end/84723 */
/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("arch=haswell", "default"))) int __tanh() {}
__typeof(__tanh) tanhf64 __attribute__((alias("__tanh"),target_clones("arch=haswell", "default"))); /* { dg-error "clones for .target_clones. attribute cannot be created" } */
  /* { dg-message "'target_clones' cannot be combined with 'alias' attribute" "" { target *-*-* } .-1 } */
