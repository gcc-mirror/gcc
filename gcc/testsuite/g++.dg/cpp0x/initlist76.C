// PR c++/58812
// { dg-require-effective-target c++11 }

int i;
int&& j{{ i }};			// { dg-error "too many braces" }
