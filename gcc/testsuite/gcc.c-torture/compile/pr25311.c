/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */

struct w
{
  int top;
  int left;
  int height;
  int width;
  struct w *next;
  struct w *parent;
  struct w *child;
};

extern struct w *Qnil;

void use (struct w *);

void
set_size (struct w *w, int new_size, int nodelete, int set_height)
{
  int old_size = set_height? w->height : w->width;

  if (nodelete || w->parent == Qnil)
    {
      int last_pos, last_old_pos, pos, old_pos, first;
      int div_val = old_size << 1;
      struct w *c;

      last_pos = first = set_height? w->top : w->left;
      last_old_pos = 0;

      for (c = w->child; c != Qnil; c = c->next)
	{
	  if (set_height)
	    old_pos = last_old_pos + c->height;
	  else
	    old_pos = last_old_pos + c->width;

	  pos = (((old_pos * new_size) << 1) + old_size) / div_val;
	  set_size (c, pos + first - last_pos, 1, set_height);
	  last_pos = pos + first;
	  last_old_pos = old_pos;
	}

      if (!nodelete)
	for (c = w->child; c != Qnil; c = c->next)
	  use (c);
    }
}

