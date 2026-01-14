// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void
fn1 ()
{
  int x = 1;	    // { dg-message ".x. declared here" }
  constexpr auto r = ^^x;

  [] -> decltype([:r:]) {
    return {};
  };

  [] -> decltype(x) {
    return [:r:];   // { dg-error "use of local variable" }
  };
}

void
fn2 ()
{
  static int x = 1;
  constexpr auto r = ^^x;

  [] -> decltype([:r:]) {
    return [:r:];
  };
}

void
fn3 ()
{
  int x = 1;	// { dg-message ".x. declared here" }

  [] -> int {
    static constexpr auto r = ^^x;  // { dg-error "cannot be applied a local entity" }
    return [:r:];
  };
}

void
fn4 ()
{
  static int x = 1;

  [] -> int {
    static constexpr auto r = ^^x;
    return [:r:];
  };
}

void
fn5 (int x)   // { dg-message ".x. declared here" }
{
  constexpr auto r = ^^x;

  [] -> decltype([:r:]) {
    return {};
  };

  [] -> decltype(x) {
    return [:r:];   // { dg-error "use of local variable" }
  };
}

void
fn6 (int x)   // { dg-message ".x. declared here" }
{
  [] -> int {
    static constexpr auto r = ^^x;  // { dg-error "cannot be applied a local entity" }
    return [:r:];
  };
}
