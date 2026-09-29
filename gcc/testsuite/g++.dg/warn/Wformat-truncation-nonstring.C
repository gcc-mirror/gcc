// A bounded %s conversion need not read a terminating nul.
// { dg-do compile { target c++11 } }
// { dg-options "-O1 -Wall" }

struct Buffer { char data[3]; };
constexpr auto buffer = Buffer{{'a', 'b', 'c'}};

int bounded ()
{
  char out[3];
  return __builtin_snprintf (out, sizeof out, "%.*s", 2,
			     buffer.data + 1); // { dg-bogus "nul-terminated" "" { target *-*-* } .-1 }
}

int bounded_literal ()
{
  char out[3];
  return __builtin_snprintf (out, sizeof out, "%.2s",
			     buffer.data + 1); // { dg-bogus "nul-terminated" "" { target *-*-* } .-1 }
}

int last_byte ()
{
  char out[2];
  return __builtin_snprintf (out, sizeof out, "%.*s", 1,
			     buffer.data + 2);
}

int overread ()
{
  char out[8];
  return __builtin_snprintf (out, sizeof out, "%.*s", 3,
			     buffer.data + 1); // { dg-warning "nul-terminated" "" { target *-*-* } .-1 }
}

int unbounded ()
{
  char out[8];
  return __builtin_snprintf (out, sizeof out, "%s",
			     buffer.data + 1); // { dg-warning "nul-terminated" "" { target *-*-* } .-1 }
}

int truncated ()
{
  char out[2];
  return __builtin_snprintf (out, sizeof out, "%.*s", 2,
			     buffer.data + 1); // { dg-warning "truncated" "" { target *-*-* } .-1 }
}
