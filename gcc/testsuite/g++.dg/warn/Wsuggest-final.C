// { dg-do compile }
// { dg-options "-O2 -Wsuggest-final-types -Wsuggest-final-methods" }
int c;
struct A { // { dg-warning "final would enable devirtualization of 4 calls" }
virtual void a() {} // { dg-warning "final would enable devirtualization of 2 calls" }
 virtual void b() {c++;} // { dg-warning "final would enable devirtualization of 2 calls"  }
};
void
t(struct A *a)
{
  a->a();
  a->a();
  a->b();
  a->b();
}
