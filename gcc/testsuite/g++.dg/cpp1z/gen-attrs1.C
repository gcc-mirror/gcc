// { dg-do compile { target c++11 } }
// { dg-options "" }

int
foo ()
{
  static int a [[using gnu: unused, used]];	// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
  int b [[ using foo : bar (2), baz ]];		// { dg-warning "'foo::bar' scoped attribute directive ignored" }
						// { dg-warning "'foo::baz' scoped attribute directive ignored" "" { target *-*-* } 8 }
						// { dg-warning "attribute using prefix only available" "" { target c++14_down } 8 }
  int c [[ using foo : using ("foo")]];		// { dg-warning "'foo::using' scoped attribute directive ignored" }
						// { dg-warning "attribute using prefix only available" "" { target c++14_down } 11 }
  b = 0;
  c = 0;
  return b + c;
}

int
bar ()
{
  int a [[ using BAR: foo::bar]];		// { dg-error "attribute using prefix used together with scoped attribute token" }
						// { dg-warning "ignored" "" { target *-*-* } 21 }
						// { dg-warning "attribute using prefix only available" "" { target c++14_down } 21 }
  int b [[ using BAZ: bar(2), bar::bar(3, 4) ]];// { dg-error "attribute using prefix used together with scoped attribute token" }
						// { dg-warning "ignored" "" { target *-*-* } 24 }
						// { dg-warning "attribute using prefix only available" "" { target c++14_down } 24 }
  a = 0;
  b = 0;
  return a + b;
}

int
baz ()
{
  int a [[ using using: using]];		// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
						// { dg-warning "'using::using' scoped attribute directive ignored" "" { target *-*-* } 35 }
  int b [[ using bitand: bitor, xor]];		// { dg-warning "attribute using prefix only available" "" { target c++14_down } }
						// { dg-warning "'bitand::bitor' scoped attribute directive ignored" "" { target *-*-* } 37 }
						// { dg-warning "'bitand::xor' scoped attribute directive ignored" "" { target *-*-* } 37 }
  a = 0;
  b = 0;
  return a + b;
}
