/* { dg-do run } */

struct list { struct list *n; };

struct obj {
    int n;
    struct list l;
} _o;

struct list _l = { .n = &_o.l };

int main(int argc, char *argv[])
{
  struct obj *o = &_o;
  _o.l.n = &_l;
  while (&o->l != &_l)
    /* Note the following is invoking undefined behavior but in
       this kind of "obvious" cases we don't want to break things
       unnecessarily and thus we avoid analyzing o as pointing
       to nothing via the undefined pointer subtraction.  Instead
       we canonicalize the pointer subtraction followed by the
       pointer conversion to pointer offsetting.  */
    o = ((struct obj *)((const char *)(o->l.n)
			- (const char *)&((struct obj *)0)->l));
  return 0;
}
