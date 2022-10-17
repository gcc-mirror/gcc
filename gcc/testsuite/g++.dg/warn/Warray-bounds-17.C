// PR middle-end/99109
// { dg-do compile }
// { dg-options "-O2 -Warray-bounds" }

typedef int A __attribute__((aligned (64)));
void foo (int *);

void
bar (void)
{
  A b;			// { dg-message "at offset -\\d into object 'b' of size 4" "note" }
  int *p = &b;
  int *x = (p - 1);	// { dg-warning "outside array bounds" }
  foo (x);
}
