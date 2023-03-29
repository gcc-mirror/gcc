// DR 2604 - Attributes for an explicit specialization.
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-parameter" }

template<typename>
[[noreturn]] void
foo ([[maybe_unused]] int i)
{
  for (;;);
}

template<>
void
foo<int> (int i)	// { dg-warning "unused parameter 'i'" }
{
}

template<typename>
void
bar (int i)		// { dg-warning "unused parameter 'i'" }
{
}

template<>
[[noreturn]] void
bar<int> ([[maybe_unused]] int i)
{
  for (;;);
}

[[noreturn]] void
baz ()
{
  foo<long> (0);
}

[[noreturn]] void
qux ()
{
  foo<int> (0);
}			// { dg-warning "'noreturn' function does return" }

[[noreturn]] void
garply ()
{
  bar<long> (0);
}			// { dg-warning "'noreturn' function does return" }

[[noreturn]] void
corge ()
{
  bar<int> (0);
}
