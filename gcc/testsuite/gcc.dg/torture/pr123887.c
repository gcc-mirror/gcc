/* { dg-do run } */

typedef struct {
    unsigned s;
} struct_t;

struct_t *p;
_Bool cond;
int v = 1;
int main()
{
  int u = 1 + (cond ? p->s : 0);
  return u - v;
}
