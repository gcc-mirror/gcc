/* PR c++/80 */
/* { dg-do compile } */

/* Used to get:
  bug.C:7: semicolon missing after declaration of `numbers'
*/
enum numbers { zero, one, two, three } __attribute__ ((packed)) ;
