// PR c++/29734
// { dg-do compile }
// { dg-options "" }

int t;
float u;
int __attribute__((vector_size (8))) v;
float __attribute__((vector_size (8))) w;
int b[10];

void
foo ()
{
  b[t];
  b[u];		// { dg-error "4:invalid types" }
  b[v];		// { dg-error "4:invalid types" }
  b[w];		// { dg-error "4:invalid types" }
  t[b];
  u[b];		// { dg-error "4:invalid types" }
  v[b];		// { dg-error "4:invalid types" }
  w[b];		// { dg-error "4:invalid types" }
  new int[t];
  new int[u];	// { dg-error "11:expression in new-declarator must have integral" }
  new int[v];	// { dg-error "11:expression in new-declarator must have integral" }
  new int[w];	// { dg-error "11:expression in new-declarator must have integral" }
  switch (t) { default: break; }
  switch (u) { default: break; }	// { dg-error "11:switch quantity not an integer" }
  switch (v) { default: break; }	// { dg-error "11:switch quantity not an integer" }
  switch (w) { default: break; }	// { dg-error "11:switch quantity not an integer" }
  t = ~t;
  u = ~u;	// { dg-error "8:wrong type argument to bit-complement" }
  v = ~v;
  w = ~w;	// { dg-error "8:wrong type argument to bit-complement" }
}
