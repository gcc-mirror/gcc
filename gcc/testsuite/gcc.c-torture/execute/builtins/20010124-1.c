/* Verify that structure return doesn't invoke memcpy on 
   overlapping objects.  */

extern void abort (void);

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

struct S f(struct S *);
void g(union U *);

void main_test(void)
{
  union U u;
  u.b.s = f(&u.a.s);
  u.a.s = f(&u.b.s);
  g(&u);
}
