/* { dg-do compile } */

typedef struct {
        struct { } z;
} thang_t;

struct {
        short           e;
        thang_t         f;
        int g;
} my_struct;

void foo (thang_t *);
void function(int blaz)
{
  thang_t *fp = &my_struct.f;
  foo(fp);
}
