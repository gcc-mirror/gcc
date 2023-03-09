/* { dg-additional-options "-O2 -Wno-shift-count-overflow" } */

struct lisp;
union vectorlike_header { long size; };
struct Lisp_Symbol { void *unused; };
extern struct Lisp_Symbol lispsym[];

static _Bool
TAGGEDP (struct lisp *a, unsigned tag)
{
  return ! (((unsigned) (long) a - tag) & 7);
}

static _Bool
VECTORLIKEP (struct lisp *x)
{
  return TAGGEDP (x, 5);
}

static _Bool
PSEUDOVECTOR_TYPEP (union vectorlike_header const *a, int code)
{
  long PSEUDOVECTOR_FLAG = 1L << 62;
  long PVEC_TYPE_MASK = 0x3fL << 24;
  return ((a->size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK))
	  == (PSEUDOVECTOR_FLAG | (code << 24)));
}

static _Bool
PSEUDOVECTORP (struct lisp *a, int code)
{
  if (! VECTORLIKEP (a))
    return 0;
  else
    return PSEUDOVECTOR_TYPEP ((union vectorlike_header *) ((char *) a - 5),
			       code);
}

static struct lisp *
builtin_lisp_symbol (int index)
{
  return (struct lisp *) (index * sizeof *lispsym);
}

static _Bool
NILP (struct lisp *x)
{
  return x == builtin_lisp_symbol (0);
}


void wrong_type_argument (struct lisp *, struct lisp *);

static void
CHECK_TYPE (int ok, struct lisp *predicate, struct lisp *x)
{
  if (!ok)
    wrong_type_argument (predicate, x);
}


struct buffer
{
  union vectorlike_header header;
  struct buffer *base_buffer;
  int window_count;
};

static _Bool
BUFFERP (struct lisp *a)
{
  return PSEUDOVECTORP (a, 12);
}

static struct buffer *
XBUFFER (struct lisp *a)
{
  return (struct buffer *) ((char *) a - 5);
}


struct window
{
  union vectorlike_header header;
  struct lisp *next;
  struct lisp *contents;
};

static _Bool
WINDOWP (struct lisp *a)
{
  return PSEUDOVECTORP (a, 12);
}

static void
CHECK_WINDOW (struct lisp *x)
{
  CHECK_TYPE (WINDOWP (x), builtin_lisp_symbol (1360), x);
}

static struct window *
XWINDOW (struct lisp *a)
{
  return (struct window *) ((char *) a - 5);
}

static void
wset_combination (struct window *w, _Bool horflag, struct lisp *val)
{
  w->contents = val;
}

extern struct lisp *selected_window;

struct window *
decode_live_window (register struct lisp *window)
{
  if (NILP (window))
    return XWINDOW (selected_window);
  CHECK_TYPE (WINDOWP (window) && BUFFERP (XWINDOW (window)->contents),
	      builtin_lisp_symbol (1351), window);
  return XWINDOW (window);
}

struct window *
decode_any_window (register struct lisp *window)
{
  struct window *w;
  if (NILP (window))
    return XWINDOW (selected_window);
  CHECK_WINDOW (window);
  w = XWINDOW (window);
  return w;
}

static void
adjust_window_count (struct window *w, int arg)
{
  if (BUFFERP (w->contents))
    {
      struct buffer *b = XBUFFER (w->contents);
      if (b->base_buffer)
	b = b->base_buffer;
      b->window_count += arg;
    }
}

void
wset_buffer (struct window *w, struct lisp *val)
{
  adjust_window_count (w, -1);
  w->contents = val;
  adjust_window_count (w, 1);
}

void
delete_all_child_windows (struct lisp *window)
{
  struct window *w = XWINDOW (window);
  if (!NILP (w->next))
    delete_all_child_windows (w->next);
  if (WINDOWP (w->contents))
    {
      delete_all_child_windows (w->contents);
      wset_combination (w, 0, builtin_lisp_symbol (0));
    }
}
