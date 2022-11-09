// { dg-do compile { target c++11 } }

[[carries_dependency]] int *f1 ();		// { dg-warning "attribute ignored" }
int f2 (int *x [[carries_dependency]]);		// { dg-warning "attribute ignored" }
[[carries_dependency]] int f3 ();		// { dg-warning "attribute ignored" }
int f4 (int x [[carries_dependency]]);		// { dg-warning "attribute ignored" }
[[carries_dependency(1)]] int f5 ();		// { dg-error "'carries_dependency' attribute does not take any arguments" }
[[carries_dependency]] int v;			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
[[carries_dependency]];				// { dg-warning "attribute ignored" }
void
f6 ()
{
  [[carries_dependency]];			// { dg-warning "attributes at the beginning of statement are ignored" }
}
#if __has_cpp_attribute(carries_dependency)
#error carries_dependency attribute is not actually implemented
#endif
