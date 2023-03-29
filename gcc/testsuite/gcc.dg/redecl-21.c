/* We used to ICE in the gimplifier, PR 106765 */
/* { dg-do compile } */
/* { dg-options "-w" } */
struct a {
  int b
} c() {
  struct a a; // { dg-note "" }
  a.b;
  d a; // { dg-error "" }
