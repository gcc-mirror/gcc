/* Verify that structure return doesn't invoke memcpy on 
   overlapping objects.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;

struct S {
  char stuff[1024];
};

union U {
  struct {
    int space;
    struct S s;
  } a;
  struct {
    struct S s;
    int space;
  } b;
};

static struct S f(struct S *);
static void g(union U *);

int main()
{
  union U u;
  u.b.s = f(&u.a.s);
  u.a.s = f(&u.b.s);
  g(&u);
  return 0;
}
  
static struct S f(struct S *p)
{
  return *p;
}

static void g(union U *p)
{
}

static void *memcpy(void *a, const void *b, size_t len)
{
  if (a < b && a+len > b)
    abort ();
  if (b < a && b+len > a)
    abort ();
  return a;
}
