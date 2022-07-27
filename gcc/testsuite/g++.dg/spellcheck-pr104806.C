// PR c++/104806

struct S {};
int main() { S s; s.__d; }	// { dg-bogus "'struct S' has no member named '__d'; did you mean '__\[a-z]* '" }
				// { dg-error "'struct S' has no member named '__d'" "" { target *-*-* } .-1 }
