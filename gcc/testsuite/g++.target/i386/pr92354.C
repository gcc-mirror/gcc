/* PR c++/92354 */
/* { dg-do compile } */
/* { dg-require-ifunc "" }  */
/* { dg-options "--param ggc-min-heapsize=0" } */

__attribute__ ((target ("default"))) void f ();
__attribute__ ((target ("sse"))) void f ();
__attribute__ ((target ("default"))) void f ();
