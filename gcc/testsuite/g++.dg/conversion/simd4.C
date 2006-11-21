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
  b[u];		// { dg-error "invalid types" }
  b[v];		// { dg-error "invalid types" }
  b[w];		// { dg-error "invalid types" }
  t[b];
  u[b];		// { dg-error "invalid types" }
  v[b];		// { dg-error "invalid types" }
  w[b];		// { dg-error "invalid types" }
  new int[t];
  new int[u];	// { dg-error "new-declarator must have integral" }
  new int[v];	// { dg-error "new-declarator must have integral" }
  new int[w];	// { dg-error "new-declarator must have integral" }
  switch (t) { default: break; }
  switch (u) { default: break; }	// { dg-error "switch quantity not an integer" }
  switch (v) { default: break; }	// { dg-error "switch quantity not an integer" }
  switch (w) { default: break; }	// { dg-error "switch quantity not an integer" }
  t = ~t;
  u = ~u;	// { dg-error "wrong type argument to bit-complement" }
  v = ~v;
  w = ~w;	// { dg-error "wrong type argument to bit-complement" }
}
