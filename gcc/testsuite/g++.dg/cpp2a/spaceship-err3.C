// { dg-do compile { target c++2a } }

namespace std
{
  int strong_ordering;
  struct partial_ordering {
    static int equivalent ();
  };
}

auto a = 1 <=> 2;	// { dg-error "'std::strong_ordering' is not a type" }
			// { dg-message "forming type of 'operator<=>'" "" { target *-*-* } .-1 }
auto b = 3.0 <=> 4.0;	// { dg-error "'std::partial_ordering::equivalent\\(\\)' is not a static data member" }
			// { dg-message "determining value of 'operator<=>'" "" { target *-*-* } .-1 }
