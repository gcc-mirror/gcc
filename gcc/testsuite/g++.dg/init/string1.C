extern void f (char*);

extern const char * const target = "foo";

void g ()
{
  f (target); // { dg-error "conversion" }
}
