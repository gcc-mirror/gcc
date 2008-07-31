/* Spurious uninitialized variable warnings, case 1.
   Taken from cppfiles.c (merge_include_chains) */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

struct list
{
  struct list *next;
  int id;
};

extern void free (void *);

void remove_dupes (struct list *el)
{
  struct list *p, *q, *r;  /* { dg-bogus "r" "uninitialized variable warning" } */

  for (p = el; p; p = p->next)
  {
    for (q = el; q != p; q = q->next)
      if (q->id == p->id)
      {
	r->next = p->next;
	free (p);
	p = r;
	break;
      }
    r = p;
  }
}
