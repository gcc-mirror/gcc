#pragma omp declare mapper (int v)  // { dg-error "missing 'map' clause before end of line" }
#pragma omp declare mapper (float v) map()  // { dg-error "expected primary-expression before '\\)' token" }
// { dg-error "'float' is not a struct, union or class type in '#pragma omp declare mapper'" "" { target *-*-* } .-1 }

#pragma omp declare mapper (char v) map(v)  // { dg-error "'char' is not a struct, union or class type in '#pragma omp declare mapper'" }

struct XT {
  int x;
};
#pragma omp declare mapper (XT y) map()  // { dg-error "expected primary-expression before '\\)' token" }

struct t {
  int x;
};

typedef struct t myStruct;

#pragma omp declare mapper(t) // { dg-error "expected unqualified-id before '\\)' token" }
#pragma omp declare mapper(struct t) // { dg-error "expected unqualified-id before '\\)' token" }
#pragma omp declare mapper(myStruct) // { dg-error "expected unqualified-id before '\\)' token" }

#pragma omp declare mapper(name : t v)  map() // { dg-error "expected primary-expression before '\\)' token" } 

#pragma omp declare mapper(fancy : struct t v) map(always,present,close,mapper(d),tofrom: v) // { dg-error "in 'declare mapper' directives, parameter to 'mapper' modifier must be 'default'" }

#pragma omp declare mapper(myStruct v) map(v, v.x)  // { dg-note "'#pragma omp declare mapper \\(myStruct\\)' previously declared here" }
#pragma omp declare mapper(default : t v) map(v, v.x) // { dg-error "redefinition of '#pragma omp declare mapper \\(t\\)'" }


class A { };
class B : public virtual A { };

#pragma omp declare mapper(class B ci) map(ci)  // { dg-error "'B' must not be a virtual base class in '#pragma omp declare mapper'" }

#pragma omp declare mapper(T v) mapper(v) // { dg-error "'T' does not name a type" }

union u_t { };

#pragma omp declare mapper(u_t v) map()  // { dg-error "expected primary-expression before '\\)' token" }
