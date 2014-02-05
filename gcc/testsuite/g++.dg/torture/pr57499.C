// PR middle-end/57499
// { dg-do compile }

struct S
{
  ~S () __attribute__ ((noreturn)) {} // { dg-warning "function does return" }
};

void
foo ()
{
  S s;
  throw 1;
}
