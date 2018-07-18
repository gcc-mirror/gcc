/* PR 64878 */
/* { dg-options "-O2" } */
/* { dg-do run } */

struct A { int a1; };
struct B { char *b1; int b2; int b3; };
struct C { char *c1; int c2; struct B *c3; };
extern struct A *f1 (char *s);
static struct A *f2 (struct C *x);
__attribute__ ((noinline, noclone)) int f3 (struct A *x, struct A *z) { asm volatile ("" : : "g" (x), "g" (z) : "memory"); return 0; }
__attribute__ ((noinline, noclone)) void f4 (struct A *x, char *y, struct A *z) { asm volatile ("" : : "g" (x), "g" (z), "g" (y) : "memory"); }
__attribute__ ((noinline, noclone)) struct B *f5 (void) { static char b[32]; static struct B f3 = { b, 0, 32 }; return &f3; }
__attribute__ ((noinline, noclone)) int f6 (struct B *p, char *w, int z) { asm volatile ("" : : "g" (p), "g" (w), "g" (z) : "memory"); return 0; }
__attribute__ ((noinline, noclone)) void f7 (struct B *p) { asm volatile ("" : : "g" (p) : "memory"); }
__attribute__ ((noinline, noclone)) void f8 (struct B *p) { asm volatile ("" : : "g" (p) : "memory"); }
__attribute__ ((noinline, noclone)) void f9 (struct A *x) { asm volatile ("" : : "g" (x) : "memory"); }
__attribute__ ((noinline, noclone)) struct A *f10 (void) { static struct A j; asm volatile ("" : :  : "memory"); return &j; }
__attribute__ ((noinline, noclone)) struct A *f11 (void) { static struct A j; asm volatile ("" : :  : "memory"); return &j; }
__attribute__ ((noinline, noclone)) struct A *f12 (int b) { static struct A j; asm volatile ("" : : "g" (b) : "memory"); return &j; }
__attribute__ ((noinline, noclone)) struct A *f13 (int i) { static struct A j; asm volatile ("" : : "g" (i) : "memory"); return &j; }
__attribute__ ((noinline, noclone)) struct A *f14 (double d) { static struct A j; asm volatile ("" : : "g" (&d) : "memory"); return &j; }
__attribute__ ((noinline, noclone)) struct A *f15 (char *s) { static struct A j; asm volatile ("" : : "g" (s) : "memory"); return &j; }
char *t = "0123456789abcdef";
char *u = "0123456789.+-e";

__attribute__ ((noinline, noclone)) struct A *
f1 (char *s)
{
  struct C f;
  struct A *o;
  f.c1 = s;
  f.c2 = 0;
  f.c3 = f5 ();
  o = f2 (&f);
  f8 (f.c3);
  return o;
}

static struct A *
f2 (struct C *x)
{
  int a, b, e = 0;
  struct A *f = 0, *o;
  char *g = 0;
  char h = '\0';
  int i = 0, j = 0;
  a = 0;
  b = 1;
  char c;
  do
    {
      c = x->c1[x->c2];
      switch (a)
	{
	case 0:
	  if (c == ' ')
	    x->c2++;
	  else if (c == '/')
	    {
	      a = 4;
	      j = x->c2++;
	    }
	  else
	    a = b;
	  break;
	case 1:
	  switch (c)
	    {
	    case '{':
	      a = 0;
	      b = 15;
	      f = f10 ();
	      x->c2++;
	      break;
	    case '[':
	      a = 0;
	      b = 13;
	      f = f11 ();
	      x->c2++;
	      break;
	    case 'N':
	    case 'n':
	      a = 3;
	      j = x->c2++;
	      break;
	    case '"':
	    case '\'':
	      h = c;
	      f7 (x->c3);
	      a = 8;
	      j = ++x->c2;
	      break;
	    case 'T':
	    case 't':
	    case 'F':
	    case 'f':
	      a = 11;
	      j = x->c2++;
	      break;
	    case '0' ... '9':
	    case '-':
	      i = 0;
	      a = 12;
	      j = x->c2++;
	      break;
	    default:
	      e = 1;
	      goto out;
	    }
	  break;
	case 2:
	  goto out;
	case 3:
	  if (__builtin_strncmp ("null", x->c1 + j, x->c2 - j))
	    {
	      e = 2;
	      goto out;
	    }
	  if (x->c2 - j == 4)
	    {
	      f = 0;
	      b = 2;
	      a = 0;
	    }
	  else
	    x->c2++;
	  break;
	case 4:
	  if (c == '*')
	    a = 5;
	  else if (c == '/')
	    a = 6;
	  else
	    {
	      e = 8;
	      goto out;
	    }
	  x->c2++;
	  break;
	case 5:
	  if (c == '*')
	    a = 7;
	  x->c2++;
	  break;
	case 6:
	  if (c == '\n')
	    a = 0;
	  x->c2++;
	  break;
	case 7:
	  if (c == '/')
	    a = 0;
	  else
	    a = 5;
	  x->c2++;
	  break;
	case 8:
	  if (c == h)
	    {
	      f6 (x->c3, x->c1 + j, x->c2 - j);
	      f = f15 (x->c3->b1);
	      b = 2;
	      a = 0;
	    }
	  else if (c == '\\')
	    {
	      b = 8;
	      a = 9;
	    }
	  x->c2++;
	  break;
	case 9:
	  switch (c)
	    {
	    case '"':
	    case '\\':
	      f6 (x->c3, x->c1 + j, x->c2 - j - 1);
	      j = x->c2++;
	      a = b;
	      break;
	    case 'b':
	    case 'n':
	    case 'r':
	    case 't':
	      f6 (x->c3, x->c1 + j, x->c2 - j - 1);
	      if (c == 'b')
		f6 (x->c3, "\b", 1);
	      else if (c == 'n')
		f6 (x->c3, "\n", 1);
	      else if (c == 'r')
		f6 (x->c3, "\r", 1);
	      else if (c == 't')
		f6 (x->c3, "\t", 1);
	      j = ++x->c2;
	      a = b;
	      break;
	    case 'u':
	      f6 (x->c3, x->c1 + j, x->c2 - j - 1);
	      j = ++x->c2;
	      a = 10;
	      break;
	    default:
	      e = 7;
	      goto out;
	    }
	  break;
	case 10:
	  if (__builtin_strchr (t, c))
	    {
	      x->c2++;
	      if (x->c2 - j == 4)
		{
		  unsigned char w[3];
		  unsigned int s =
		    (((x->c1[j] <= '9') ? x->c1[j] - '0' : (x->c1[j] & 7) + 9) << 12)
		    + (((x->c1[j + 1] <= '9') ? x->c1[j + 1] - '0' : (x->c1[j + 1] & 7) + 9) << 8)
		    + (((x->c1[j + 2] <= '9') ? x->c1[j + 2] - '0' : (x->c1[j + 2] & 7) + 9) << 4)
		    + ((x->c1[j + 3] <= '9') ? x->c1[j + 3] - '0' : (x->c1[j + 3] & 7) + 9);
		  if (s < 0x80)
		    {
		      w[0] = s;
		      f6 (x->c3, (char *) w, 1);
		    }
		  else if (s < 0x800)
		    {
		      w[0] = 0xc0 | (s >> 6);
		      w[1] = 0x80 | (s & 0x3f);
		      f6 (x->c3, (char *) w, 2);
		    }
		  else
		    {
		      w[0] = 0x0 | (s >> 12);
		      w[1] = 0x80 | ((s >> 6) & 0x3f);
		      w[2] = 0x80 | (s & 0x3f);
		      f6 (x->c3, (char *) w, 3);
		    }
		  j = x->c2;
		  a = b;
		}
	    }
	  else
	    {
	      e = 7;
	      goto out;
	    }
	  break;
	case 11:
	  if (__builtin_strncmp ("true", x->c1 + j, x->c2 - j) == 0)
	    {
	      if (x->c2 - j == 4)
		{
		  f = f12 (1);
		  b = 2;
		  a = 0;
		}
	      else
		x->c2++;
	    }
	  else if (__builtin_strncmp ("false", x->c1 + j, x->c2 - j) == 0)
	    {
	      if (x->c2 - j == 5)
		{
		  f = f12 (0);
		  b = 2;
		  a = 0;
		}
	      else
		x->c2++;
	    }
	  else
	    {
	      e = 3;
	      goto out;
	    }
	  break;
	case 12:
	  if (!c || !__builtin_strchr (u, c))
	    {
	      if (!i)
		f = f13 (0);
	      else
		f = f14 (0.0);
	      b = 2;
	      a = 0;
	    }
	  else
	    {
	      if (c == '.' || c == 'e')
		i = 1;
	      x->c2++;
	    }
	  break;
	case 13:
	  if (c == ']')
	    {
	      x->c2++;
	      b = 2;
	      a = 0;
	    }
	  else
	    {
	      o = f2 (x);
#if __SIZEOF_POINTER__ == __SIZEOF_LONG__
	      if (((unsigned long) o > (unsigned long) -4000L))
#elif __SIZEOF_POINTER__ == __SIZEOF_INT__
	  if (((__UINTPTR_TYPE__) o > (__UINTPTR_TYPE__) -4000U))
#endif
		{
		  e = 5;
		  goto out;
		}
	      f3 (f, o);
	      b = 14;
	      a = 0;
	    }
	  break;
	case 14:
	  if (c == ']')
	    {
	      x->c2++;
	      b = 2;
	      a = 0;
	    }
	  else if (c == ',')
	    {
	      x->c2++;
	      b = 13;
	      a = 0;
	    }
	  else
	    {
	      f9 (f);
	      e = 5;
	      goto out;
	    }
	  break;
	case 15:
	  a = 16;
	  j = x->c2;
	  break;
	case 16:
	  if (c == '}')
	    {
	      x->c2++;
	      b = 2;
	      a = 0;
	    }
	  else if (c == '"' || c == '\'')
	    {
	      h = c;
	      f7 (x->c3);
	      a = 17;
	      j = ++x->c2;
	    }
	  else
	    {
	      e = 6;
	      goto out;
	    }
	  break;
	case 17:
	  if (c == h)
	    {
	      f6 (x->c3, x->c1 + j, x->c2 - j);
	      g = __builtin_strdup (x->c3->b1);
	      b = 18;
	      a = 0;
	    }
	  else if (c == '\\')
	    {
	      b = 17;
	      a = 9;
	    }
	  x->c2++;
	  break;
	case 18:
	  if (c == ':')
	    {
	      x->c2++;
	      b = 19;
	      a = 0;
	    }
	  else
	    {
	      e = -6;
	      goto out;
	    }
	  break;
	case 19:
	  o = f2 (x);
#if __SIZEOF_POINTER__ == __SIZEOF_LONG__
	  if (((unsigned long) o > (unsigned long) -4000L))
#elif __SIZEOF_POINTER__ == __SIZEOF_INT__
	  if (((__UINTPTR_TYPE__) o > (__UINTPTR_TYPE__) -4000U))
#endif
	    {
	      e = 6;
	      goto out;
	    }
	  f4 (f, g, o);
	  __builtin_free (g);
	  g = 0;
	  b = 20;
	  a = 0;
	  break;
	case 20:
	  if (c == '}')
	    {
	      x->c2++;
	      b = 2;
	      a = 0;
	    }
	  else if (c == ',')
	    {
	      x->c2++;
	      b = 15;
	      a = 0;
	    }
	  else
	    {
	      e = 6;
	      goto out;
	    }
	  break;
	}
    }
  while (c);
  if (a != 2 && b != 2)
    e = 9;
out:
  __builtin_free (g);
  if (e == 0)
    return f;
  f9 (f);
  return 0;
}

int
main ()
{
  asm volatile ("" : : : "memory");
  struct A *r = f1 ("{ \"id\": null, \"blahah\": \"foobarbazbar\", \"barbar\": { \"barbarbarba\":"
		    "\"abcdefgh\", \"ijklmnopqr\": \"stuvwxyzabcdefghijklmnopqrstuv\", \"xyzxyz\":"
		    " [ \"1\" ] } }");
  if (!r)
    __builtin_abort ();
  return 0;
}
