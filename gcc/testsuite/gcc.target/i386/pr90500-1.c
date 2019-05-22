/* PR middle-end/84723 */
/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("arch=haswell", "default"))) int __tanh() {}
__typeof(__tanh) tanhf64 __attribute__((alias("__tanh")))
__attribute__((__copy__(__tanh)));
