/* { dg-require-effective-target ptr_eq_long } */
/* { dg-additional-options "-O2 -Wno-shift-count-overflow" } */

struct lisp;
union vectorlike_header { long size; };
struct Lisp_Symbol { void *unused; };
extern struct Lisp_Symbol lispsym[];
struct Lisp_Cons { struct lisp *cdr; };

static struct Lisp_Cons *
XCONS (struct lisp *a)
{
  return (struct Lisp_Cons *) ((char *) a - 3);
}

static struct lisp *
XCDR (struct lisp *c)
{
  return XCONS (c)->cdr;
}

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
  return ((a->size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK)) /* { dg-bogus "dereference of NULL 'time'" "PR analyzer/107526" { xfail *-*-* } } */
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

static _Bool
FIXNUMP (struct lisp *x)
{
  return ! (((unsigned) (long) x - 2) & 3);
}

static _Bool
BIGNUMP (struct lisp *x)
{
  return PSEUDOVECTORP (x, 2);
}

void some_function ();

static void
decode_time_components (struct lisp *low)
{
  if (BIGNUMP (low))
    some_function ();
}

_Bool
Ftime_convert (struct lisp *time)
{
  decode_time_components (time ? XCDR (time) : time);
  return BIGNUMP (time);
}
