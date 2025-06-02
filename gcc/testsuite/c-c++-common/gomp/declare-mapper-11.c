#pragma omp declare mapper (int v)
// { dg-error "missing 'map' clause before end of line" "" { target c++ } .-1 }
// { dg-error "'int' is not a struct or union type in '#pragma omp declare mapper'" "" { target c } .-2 }

#pragma omp declare mapper (float v) map()
// { dg-error "expected primary-expression before '\\)' token" "" { target c++ } .-1 }
// { dg-error "'float' is not a struct, union or class type in '#pragma omp declare mapper'" "" { target c++ } .-2 }
// { dg-error "'float' is not a struct or union type in '#pragma omp declare mapper'" "" { target c } .-3 }

#pragma omp declare mapper (char v) map(v)
// { dg-error "'char' is not a struct, union or class type in '#pragma omp declare mapper'" "" { target c++ } .-1 }
// { dg-error "'char' is not a struct or union type in '#pragma omp declare mapper'" "" { target c } .-2 }

struct XT {
  int x;
};
#pragma omp declare mapper (XT y) map()
// { dg-error "expected primary-expression before '\\)' token" "" { target c++ } .-1 }
// { dg-error "unknown type name 'XT'" "" { target c } .-2 }
// { dg-error "expected end of line before 'y'" "" { target c } .-3 }
#pragma omp declare mapper ( bar : struct XT y) map()
// { dg-error "expected primary-expression before '\\)' token" "" { target c++ } .-1 }
// { dg-error "expected expression before '\\)' token" "" { target c } .-2 }

struct t {
  int x;
};

typedef struct t myStruct;

#pragma omp declare mapper(t)
// { dg-error "expected unqualified-id before '\\)' token" "" { target c++ } .-1 }
// { dg-error "unknown type name 't'" "" { target c } .-2 }
// { dg-error "expected end of line before '\\)' token" "" { target c } .-3 }
#pragma omp declare mapper(struct t)
// { dg-error "expected unqualified-id before '\\)' token" "" { target c++ } .-1 }
// { dg-error "expected identifier" "" { target c } .-2 }
// { dg-error "expected end of line before '\\)' token" "" { target c } .-3 }
#pragma omp declare mapper(myStruct)
// { dg-error "expected unqualified-id before '\\)' token" "" { target c++ } .-1 }
// { dg-error "expected identifier" "" { target c } .-2 }
// { dg-error "expected end of line before '\\)' token" "" { target c } .-3 }

#pragma omp declare mapper(name : t v)  map()
// { dg-error "expected primary-expression before '\\)' token" "" { target c++ } .-1 }
// { dg-error "unknown type name 't'" "" { target c } .-2 }
// { dg-error "expected end of line before 'v'" "" { target c } .-3 }

#pragma omp declare mapper(fancy : struct t v) map(always,present,close,mapper(d),tofrom: v) // { dg-error "in 'declare mapper' directives, parameter to 'mapper' modifier must be 'default'" }

#pragma omp declare mapper(myStruct v) map(v, v.x)
// { dg-note "'#pragma omp declare mapper \\(myStruct\\)' previously declared here" "" { target c++ } .-1 }
// { dg-note "'#pragma omp declare mapper' previously declared here" "" { target c } .-2 }
#pragma omp declare mapper(default : struct t v) map(v, v.x)
// { dg-error "redefinition of '#pragma omp declare mapper \\(t\\)'" "" { target c++ } .-1 }
// { dg-error "redeclaration of '<default>' '#pragma omp declare mapper' for type 'struct t'" "" { target c } .-2 }

union u_t { };
union u_q { };

#pragma omp declare mapper(union u_t v) map()
// { dg-error "expected primary-expression before '\\)' token" "" { target c++ } .-1 }
// { dg-error "expected expression before '\\)' token" "" { target c } .-2 }

#pragma omp declare mapper( one : union u_t v) map(v)
// { dg-note "'#pragma omp declare mapper \\(one: u_t\\)' previously declared here" "" { target c++ } .-1 }
// { dg-note "'#pragma omp declare mapper' previously declared here" "" { target c } .-2 }
#pragma omp declare mapper( one : union u_t u) map( u )
// { dg-error "redefinition of '#pragma omp declare mapper \\(one: u_t\\)'" "" { target c++ } .-1 }
// { dg-error "redeclaration of 'one' '#pragma omp declare mapper' for type 'union u_t'" "" { target c } .-2 }
