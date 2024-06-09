/* Test the attribute counted_by and its usage in
 * __builtin_dynamic_object_size: when the type of the flexible array member
 * is casting to another type.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

typedef unsigned short u16;

struct info {
       u16 data_len;
       char data[] __attribute__((counted_by(data_len)));
};

struct foo {
       int a;
       int b;
};

static __attribute__((__noinline__))
struct info *setup ()
{
 struct info *p;
 size_t bytes = 3 * sizeof(struct foo);

 p = (struct info *)malloc (sizeof (struct info) + bytes);
 p->data_len = bytes;

 return p;
}

static void
__attribute__((__noinline__)) report (struct info *p)
{
 struct foo *bar = (struct foo *)p->data;
 EXPECT(__builtin_dynamic_object_size((char *)(bar + 1), 1), 16);
 EXPECT(__builtin_dynamic_object_size((char *)(bar + 2), 1), 8);
}

int main(int argc, char *argv[])
{
 struct info *p = setup();
 report(p);
 return 0;
}
