/* { dg-do compile } */

struct T;

struct S {
  void (*bar)(struct S);
};

void bar(struct T t) {} /* { dg-error "" }  */

void foo(struct S *s)
{
  s->bar = bar;
}

