// PR c++/91110
// { dg-do compile }

void
foo ()
{
  X b[2];	// { dg-error "'X' was not declared in this scope" }
  b[0] = 1;	// { dg-error "'b' was not declared in this scope" }
  #pragma omp target map(to: b)	// { dg-error "'b' does not have a mappable type in 'map' clause" }
  ;
}
