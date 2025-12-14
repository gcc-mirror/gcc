/* { dg-do compile } */
/* { dg-options "-Os -fexceptions -finstrument-functions-once" } */
/* PR tree-optimization/123110 */

/* Make sure forwarder blocks (with labels) to eh landing pads are handled
   correctly. */

void baz ();

int g, h;

static inline __attribute__((__always_inline__)) void
bar (int *out_base)
{
  if (!g)
    {
      baz ();
      switch (h)
	;
      return;
    }
fail:
  *out_base = 0;
}

void
foo ()
{
  unsigned base;
  bar (&base);
}
