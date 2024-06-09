typedef struct A A;
typedef struct B B;
struct A { char *a; long b; };
enum { C, D };
typedef struct { A *c; A *d; } E;
typedef enum { F } G;
typedef enum { H } I;
struct B { A *e, *f, *g, *h; char i; } j;
int k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af;
int ag, ah, ai, aj, ak, al, am, an, ao, ap;
E aq;
G ar;
I as;
char at;

static int
foo (char *c, long d)
{
  switch (d) {
  case 7:
    switch (c[6])
    case 'd':
      if (ao)
      case 'h':
        if (an)
        case 'r':
          if (am)
          case 's':
            if (al)
              if (ak)
                return C;
    /* FALLTHRU */
  case 8:
    switch (c[7])
    case 'e':
      if (aj)
      case 'h':
        if (ai)
        case 'n':
          if (ah)
          case 'y':
            if (ag)
            case 9:
              switch (c[8])
              case 'l':
              case 0:
                switch (c[9])
                case 'e':
                  if (af)
                    if (ae)
                    case 'n':
                      if (ad)
                      case 't':
                        if (ac)
                        case 'y':
                          if (ab)
                          case 1:
                            switch (c[0])
                            case 'r':
                            case 2:
                              switch (c[1])
                              case 'e':
                              case 3:
                                switch (c[2])
                                case 'd':
                                  if (aa)
                                  case 'e':
                                    if (z)
                                    case 'h':
                                      if (y)
                                      case 'l':
                                        if (x)
                                        case 'n':
                                          if (w)
                                          case 's':
                                            if (v)
                                            case 4:
                                              switch (c[3])
                                              case 'h':
                                                if (u)
                                                case 't':
                                                  if (t)
                                                  case 5:
                                                    switch (c[4])
                                                    case 'e':
                                                      if (s)
                                                      case 'g':
                                                        if (r)
                                                        case 6:
                                                          switch (c[5])
                                                          case 'e':
                                                            if (q)
                                                              if (p)
                                                              case 'g':
                                                                if (o)
                                                                case 'n':
                                                                  if (n)
                                                                    if (m)
                                                                    case 7:
                                                                      switch (c[6])
                                                                      case 'e':
                                                                        if (l)
                                                                        case 'g':
                                                                          if (k)
                                                                            return D;
  }
  return 0;
}

void bar (void);

static int
baz (B *x)
{
  aq.c = x->e;
  aq.d = x->f;
  ap = foo (x->e->a, x->e->b);
  if (x->i)
    bar ();
  x->g = aq.c;
  x->h = aq.d;
  return 0;
}

void
qux (void)
{
  for (; at;)
    switch (as)
      {
      case H:
	baz (&j);
	j.f->b = 0;
	if (ar)
	  baz (&j);
      }
}
