/* PR c/26818 */
/* { dg-do compile } */

struct __attribute__ ((packed)) A
{
  struct B b; /* { dg-error "incomplete" } */
};
