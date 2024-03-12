struct s1 {
  time_t mytime; /* { dg-error "unknown type name 'time_t'" "c error" { target c } } */
  /* { dg-error "'time_t' does not name a type" "c++ error" { target c++ } .-1 } */
  /* { dg-message "'time_t' is defined in header" "hint" { target *-*-* } .-2 } */
};

struct s2 {
  unsinged i; /* { dg-error "unknown type name 'unsinged'; did you mean 'unsigned'." "c error" { target c } } */
  /* { dg-error "'unsinged' does not name a type; did you mean 'unsigned'." "c++ error" { target c++ } .-1 } */
};
