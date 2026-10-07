// Tests the warnings for insufficient allocation size.
// { dg-do compile }
// { dg-options "-Walloc-size" }

struct S { int x[10]; };
void bar (S *);

void
foo (void)
{
  S *p = new S[0];
  char *q = new char[0];
  S *r = (S *) ::operator new[] (0);	// { dg-warning "allocation of insufficient size '0' for type 'S' with size '\[0-9]\+'" }
  char *t = (char *) ::operator new[] (0); // { dg-warning "allocation of insufficient size '0' for type 'char' with size '1'" }
  S *u = (S *) ::operator new[] (1);	// { dg-warning "allocation of insufficient size '1' for type 'S' with size '\[0-9]\+'" }
  ::operator delete[] (u);
  ::operator delete[] (t);
  ::operator delete[] (r);
  delete[] q;
  delete[] p;
}
