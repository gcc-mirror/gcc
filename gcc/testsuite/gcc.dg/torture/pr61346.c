/* { dg-do run } */

extern void abort (void);

typedef int int32_t __attribute__ ((mode (SI)));
typedef int int64_t __attribute__ ((mode (DI)));
typedef __SIZE_TYPE__ size_t;

struct slice
{
  unsigned char *data;
  int64_t len;
  int64_t cap;
};

void fail (int32_t) __attribute__ ((noinline));
void
fail (int32_t c)
{
  if (c != 0)
    abort ();
}

struct decode_rune_ret
{
  int32_t r;
  int64_t width;
};

struct decode_rune_ret decode_rune (struct slice) __attribute__ ((noinline));
struct decode_rune_ret
decode_rune (struct slice s)
{
  struct decode_rune_ret dr;
  dr.r = s.data[0];
  dr.width = 1;
  return dr;
}

_Bool is_space (int32_t) __attribute__ ((noinline));
_Bool
is_space (int32_t r)
{
  return r == ' ';
}

struct ret
{
  int64_t advance;
  struct slice token;
};

struct ret scanwords (struct slice, _Bool) __attribute__ ((noinline));

struct ret
scanwords (struct slice data, _Bool ateof)
{
  int64_t advance;
  struct slice token;
  int64_t start = 0;
  {
    int64_t width;
    for (width = 0; start < data.len; start += width)
      {
	int32_t r = 0;
	struct slice s;
	if (start > data.cap || start < 0)
	  fail (3);
	s.data = data.data + (size_t) start;
	s.len = data.len - start;
	s.cap = data.cap - start;
	struct decode_rune_ret dr = decode_rune (s);
	r = dr.r;
	width = dr.width;
	if (!is_space (r))
	  break;
      }
  }
  _Bool tmp = ateof;
  if (tmp != 0)
    goto L1;
  else
    goto L2;
 L1:
  tmp = data.len == 0;
 L2:
  if (tmp != 0)
    goto L11;
  else
    goto L12;
 L11:
    {
      struct ret r;
      advance = 0;
      token.data = 0;
      token.len = 0;
      token.cap = 0;
      r.advance = advance;
      r.token = token;
      return r;
    }
 L12:;
  int64_t width;
  int64_t i;
  for (width = 0, i = start; i < data.len; i += width)
    {
      int32_t r;
      struct slice s;
      if (i > data.cap || i < 0)
	fail (3);
      s.data = data.data + i;
      s.len = data.len - i;
      s.cap = data.cap - i;
      struct decode_rune_ret dr = decode_rune (s);
      r = dr.r;
      width = dr.width;
      if (is_space (r))
	{
	  if (i < start || i > data.cap || i < 0)
	    fail (3);
	  if (start > data.cap || start < 0)
	    fail (3);
	  struct ret r;
	  advance = i + width;
	  token.data = data.data + (size_t) start;
	  token.len = i - start;
	  token.cap = data.cap - start;
	  r.advance = advance;
	  r.token = token;
	  return r;
	}
    }
  {
    struct ret r;
    advance = 0;
    token.data = 0;
    token.len = 0;
    token.cap = 0;
    r.advance = advance;
    r.token = token;
    return r;
  }
}

int
main ()
{
  unsigned char buf[1000];
  struct slice s;
  __builtin_memset (buf, 0, sizeof (buf));
  buf[0] = ' ';
  buf[1] = 'a';
  buf[2] = ' ';
  s.data = buf;
  s.len = 3;
  s.cap = sizeof (buf);
  struct ret r;
  r = scanwords (s, 1);
  if (r.advance != 3 || r.token.data[0] != 'a' || r.token.len != 1)
    abort ();
  return 0;
}
