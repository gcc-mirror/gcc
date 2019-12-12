// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 10334

module ddoc10334;

template Foo10334(T) if (Bar10334!()) {}                ///
template Foo10334(T) if (Bar10334!100) {}               ///
template Foo10334(T) if (Bar10334!3.14) {}              ///
template Foo10334(T) if (Bar10334!"str") {}             ///
template Foo10334(T) if (Bar10334!1.4i) {}              ///
template Foo10334(T) if (Bar10334!null) {}              ///
template Foo10334(T) if (Bar10334!true) {}              ///
template Foo10334(T) if (Bar10334!false) {}             ///
template Foo10334(T) if (Bar10334!'A') {}               ///
template Foo10334(T) if (Bar10334!int) {}               ///
template Foo10334(T) if (Bar10334!string) {}            ///
template Foo10334(T) if (Bar10334!([1,2,3])) {}         ///
template Foo10334(T) if (Bar10334!(Baz10334!())) {}     ///
template Foo10334(T) if (Bar10334!(Baz10334!T)) {}      ///
template Foo10334(T) if (Bar10334!(Baz10334!100)) {}    ///
template Foo10334(T) if (Bar10334!(.foo)) {}            ///
template Foo10334(T) if (Bar10334!(const int)) {}       ///
template Foo10334(T) if (Bar10334!(shared T)) {}        ///

template Test10334(T...) {}     ///
mixin Test10334!int a;          ///
mixin Test10334!(int,long) b;   ///
mixin Test10334!"str" c;        ///
