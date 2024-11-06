// { dg-do compile }

#pragma omp declare variant (f1) match(user={condition(1)})	// { dg-error "'f1' was not declared in this scope; did you mean 'f2'\\\?" }
void
f2 (int)
{
}

void f3 (int);

#pragma omp declare variant (f3) match(user={condition(1)})	// { dg-error "variant 'void f3\\\(int\\\)' and base 'void f4\\\(long int\\\)' have incompatible types" }
void
f4 (long)
{
}

#pragma omp declare variant (f5) match(user={condition(1)})	// { dg-error "there are no arguments to 'f5' that depend on a template parameter, so a declaration of 'f5' must be available" }
template <int N>
void
f6 (int)
{
}

template <int N>
void f7 (int);

#pragma omp declare variant (f7) match(user={condition(1)})	// { dg-error "no matching function for call to 'f7\\\(long int\\\)'" }
template <int N>
void
f8 (long)
{
}

#pragma omp declare variant (f9) match(user={condition(1)})
template <typename T>
void
f10 (T)								// { dg-error "'f9' was not declared in this scope; did you mean 'f8'\\\?" }
{
}

template <typename T>
void f11 (T, int);

#pragma omp declare variant (f11) match(user={condition(1)})	// { dg-error "variant 'void f11\\\(T, int\\\) \\\[with T = int\\\]' and base 'void f12\\\(T, long int\\\) \\\[with T = int\\\]' have incompatible types" }
template <typename T>
void
f12 (T, long)
{
}

void
test ()
{
  f10 (0);							// { dg-error "no matching function for call to 'f10\\\(int\\\)'" }
  f12 (0, 0L);
}
