/* { dg-do compile } */
/* { dg-options "-O2" } */

struct pgm_slist_t
{
  struct pgm_slist_t *__restrict next;
};

void
fn1 (struct pgm_slist_t p1)
{

}
