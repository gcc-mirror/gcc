// PR c++/85515
// { dg-do compile { target c++11 } }

int a[10];

void
foo ()
{
  for (auto &i : a)
    if (i != *__for_begin		// { dg-error "was not declared in this scope" }
	|| &i == __for_end		// { dg-error "was not declared in this scope" }
	|| &__for_range[0] != &a[0])	// { dg-error "was not declared in this scope" }
      __builtin_abort ();
}

template <int N>
void
bar ()
{
  for (auto &i : a)
    if (i != *__for_begin		// { dg-error "was not declared in this scope" }
	|| &i == __for_end		// { dg-error "was not declared in this scope" }
	|| &__for_range[0] != &a[0])	// { dg-error "was not declared in this scope" }
      __builtin_abort ();
}

void
baz ()
{
  foo ();
  bar <0> ();
}
