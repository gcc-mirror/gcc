// { dg-do compile }

template<typename T>
void foo ()
{
  T *ptr;

#pragma omp target update to(([5][6][7]) ptr[0:4][0:7][0:7])
// { dg-error {length '7' with stride '1' above array section size in 'to' clause} "" { target *-*-* } .-1 }

#pragma omp target update to(([5][6][7]) ptr[1:5][0:6][0:7])
// { dg-error {high bound '6' above array section size in 'to' clause} "" { target *-*-* } .-1 }

  // This one's OK...
#pragma omp target update from(([100]) ptr[3:33:3])

  // But this is one element out of bounds.
#pragma omp target update from(([100]) ptr[4:33:3])
// { dg-error {high bound '101' above array section size in 'from' clause} "" { target *-*-* } .-1 }

#pragma omp target update to(([10][10]) ptr[0:9:-1][0:9])
// { dg-error {length '9' with stride '-1' above array section size in 'to' clause} "" { target *-*-* } .-1 }
}

int main()
{
  char *ptr;

#pragma omp target update to(([5][6][7]) ptr[0:4][0:7][0:7])
// { dg-error {length '7' with stride '1' above array section size in 'to' clause} "" { target *-*-* } .-1 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 }

#pragma omp target update to(([5][6][7]) ptr[1:5][0:6][0:7])
// { dg-error {high bound '6' above array section size in 'to' clause} "" { target *-*-* } .-1 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 }

#pragma omp target update from(([100]) ptr[3:33:3])

#pragma omp target update from(([100]) ptr[4:33:3])
// { dg-error {high bound '101' above array section size in 'from' clause} "" { target *-*-* } .-1 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 }

#pragma omp target update to(([10][10]) ptr[0:9:-1][0:9])
// { dg-error {length '9' with stride '-1' above array section size in 'to' clause} "" { target *-*-* } .-1 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 }

  foo<char> ();

  return 0;
}
