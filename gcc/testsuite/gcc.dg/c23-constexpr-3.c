/* Test C23 constexpr.  Invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

extern constexpr int v0 = 0; /* { dg-error "'constexpr' used with 'extern'" } */
/* { dg-warning "initialized and declared 'extern'" "initialized extern" { target *-*-* } .-1 } */
constexpr extern int v1 = 0; /* { dg-error "'constexpr' used with 'extern'" } */
/* { dg-warning "initialized and declared 'extern'" "initialized extern" { target *-*-* } .-1 } */
typedef constexpr int v2; /* { dg-error "'constexpr' used with 'typedef'" } */
constexpr typedef int v3; /* { dg-error "'constexpr' used with 'typedef'" } */
thread_local constexpr int v4 = 0; /* { dg-error "'constexpr' used with '_Thread_local'" } */
constexpr thread_local int v5 = 0; /* { dg-error "'thread_local' used with 'constexpr'" } */
constexpr constexpr int v6 = 1; /* { dg-error "duplicate 'constexpr'" } */
constexpr struct v7; /* { dg-error "'constexpr' in empty declaration" } */
/* { dg-error "'struct v7' declared in underspecified object declaration" "underspecified" { target *-*-* } .-1 } */
constexpr union v8; /* { dg-error "'constexpr' in empty declaration" } */
/* { dg-error "'union v8' declared in underspecified object declaration" "underspecified" { target *-*-* } .-1 } */
constexpr struct v9 { int a; }; /* { dg-error "'constexpr' in empty declaration" } */
/* { dg-error "'struct v9' defined in underspecified object declaration" "underspecified" { target *-*-* } .-1 } */
constexpr union v10 { int a; }; /* { dg-error "'constexpr' in empty declaration" } */
/* { dg-error "'union v10' defined in underspecified object declaration" "underspecified" { target *-*-* } .-1 } */
constexpr; /* { dg-error "'constexpr' in empty declaration" } */
constexpr int; /* { dg-error "empty declaration" } */
constexpr const; /* { dg-error "empty declaration" } */
constexpr int v11; /* { dg-error "initialized data declaration" } */
constexpr int v12 { } /* { dg-error "initialized data declaration" } */
constexpr int v13 = 1, v14 = 2; /* { dg-error "single declarator" } */
constexpr int v15 = sizeof (struct v16 *); /* { dg-error "declared in underspecified object initializer" } */
constexpr int v17 = sizeof (union v18 *); /* { dg-error "declared in underspecified object initializer" } */
constexpr int v19 = sizeof (struct v20 { int a; }); /* { dg-error "defined in underspecified object initializer" } */
constexpr int v21 = sizeof (struct { int a; }); /* { dg-error "defined in underspecified object initializer" } */
constexpr int v22 = sizeof (union v23 { int a; }); /* { dg-error "defined in underspecified object initializer" } */
constexpr int v24 = sizeof (union { int a; }); /* { dg-error "defined in underspecified object initializer" } */
constexpr int v25 = sizeof (enum v26 { A }); /* { dg-error "defined in underspecified object initializer" } */
/* The following case is undefined behavior (so doesn't actually require a
   diagnostic).  */
constexpr int v27 = sizeof (enum { B }); /* { dg-error "defined in underspecified object initializer" } */
/* Examples with a forward declaration, then definition inside constexpr.  */
struct v28;
constexpr int v29 = sizeof (struct v28 { int a; }); /* { dg-error "defined in underspecified object initializer" } */
union v30;
constexpr int v31 = sizeof (union v30 { int a; }); /* { dg-error "defined in underspecified object initializer" } */
constexpr int v32 = sizeof (v32); /* { dg-error "underspecified 'v32' referenced in its initializer" } */
static const int v33;
constexpr const int v33 = 1; /* { dg-error "underspecified declaration of 'v33', which is already declared in this scope" } */
constexpr void v34 () {} /* { dg-error "'constexpr' requires an initialized data declaration" } */
void v35 (constexpr int v36); /* { dg-error "storage class specified for parameter 'v36'" } */
void v37 (constexpr short); /* { dg-error "storage class specified for unnamed parameter" } */
void v38 (constexpr register int v39); /* { dg-error "storage class specified for parameter 'v39'" } */
void v40 (constexpr register short); /* { dg-error "storage class specified for unnamed parameter" } */
/* The following case is undefined behavior (presumably to allow for possible
   future support for constexpr functions), but should clearly be diagnosed
   when such functions aren't actually supported.  */
constexpr int v41 (); /* { dg-error "'constexpr' requires an initialized data declaration" } */
typedef volatile long t42;
typedef int *restrict t43;
typedef _Atomic int t44;
struct t45 { struct { struct { t42 a[2]; } a; } a; };
struct t46 { struct { struct { int z; int *restrict a; } a[3]; } a; };
struct t47 { short x; struct { struct { _Atomic long a; } a; } a[4][5]; };
constexpr t42 v48 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr t43 v49 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr t44 v50 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr volatile double v51 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr int *restrict v52 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr _Atomic (short) v53 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr long *volatile v54 = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr struct t45 v55 = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
constexpr struct t46 v56 = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
constexpr struct t47 v57 = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
union t58 { struct { union { t42 a[1]; } a; } a; };
union t59 { struct { union { int z; int *restrict a; } a; } a; };
union t60 { short x; union { struct { _Atomic long a; } a[3]; } a; };
constexpr union t58 v61 = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
constexpr union t59 v62 = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
constexpr union t60 v63 = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
constexpr t42 v64[1][2][3] = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr volatile int v65[1][2][3] = {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
constexpr struct t45 v66[2][2][4] = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
constexpr union t60 v67[2][2][4] = {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
int v68 = 0;
constexpr int v69 = v68; /* { dg-error "initializer element is not constant" } */
double exp (double);
constexpr double v70 = exp (0); /* { dg-error "initializer element is not a constant expression" } */
struct s71 { int a; double b; };
constexpr struct s71 v72 = { 0, exp (0) }; /* { dg-error "initializer element is not a constant expression" } */
/* { dg-error "'constexpr' initializer is not an arithmetic constant expression" "arithmetic" { target *-*-* } .-1 } */
constexpr struct s71 v73 = { v68, 0 }; /* { dg-error "initializer element is not constant" } */
union u74 { int a; double b; };
constexpr union u74 v75 = { v68 }; /* { dg-error "initializer element is not constant" } */
constexpr union u74 v76 = { .b = exp (0) }; /* { dg-error "initializer element is not a constant expression" } */
/* { dg-error "'constexpr' initializer is not an arithmetic constant expression" "arithmetic" { target *-*-* } .-1 } */
constexpr struct s77 *v77 = 0; /* { dg-error "'struct s77' declared in underspecified object declaration" } */
constexpr union u78 *v78 = 0; /* { dg-error "'union u78' declared in underspecified object declaration" } */
constexpr struct s79 { int a; } v79 = { 0 }; /* { dg-error "'struct s79' defined in underspecified object declaration" } */
constexpr union u80 { int a; } v80 = { 0 }; /* { dg-error "'union u80' defined in underspecified object declaration" } */
constexpr enum e81 { E81 } v81 = E81; /* { dg-error "'enum e81' defined in underspecified object declaration" } */
constexpr enum { E82 } v82 = E82; /* { dg-error "defined in underspecified object declaration" } */
struct s83 constexpr *v83 = 0; /* { dg-error "'struct s83' declared in underspecified object declaration" } */
union u84 constexpr *v84 = 0; /* { dg-error "'union u84' declared in underspecified object declaration" } */
struct s85 { int a; } constexpr v85 = { 0 }; /* { dg-error "'struct s85' defined in underspecified object declaration" } */
union u86 { int a; } constexpr v86 = { 0 }; /* { dg-error "'union u86' defined in underspecified object declaration" } */
enum e87 { E87 } constexpr v87 = E87; /* { dg-error "'enum e87' defined in underspecified object declaration" } */
enum { E88 } constexpr v88 = E88; /* { dg-error "defined in underspecified object declaration" } */
constexpr void *v89 = (void *) 64; /* { dg-error "'constexpr' pointer initializer is not null" } */
constexpr int *v90 = (int *) 64; /* { dg-error "'constexpr' pointer initializer is not null" } */
constexpr int v91 = (int) (double) 1.0; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */
constexpr struct s71 v92 = { (int) (double) 1.0, 0 }; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */
struct s93 { void *p; };
constexpr struct s93 v94 = { (int *) 16 }; /* { dg-error "'constexpr' pointer initializer is not null" } */
constexpr int v95 = (unsigned int) -1; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr unsigned char v96 = -1; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr signed char v97 = 1234567LL; /* { dg-error "'constexpr' initializer not representable in type of object" } */
/* { dg-warning "overflow in conversion" "overflow warning" { target *-*-* } .-1 } */
constexpr double v98 = __builtin_complex (1.0, 0.0); /* { dg-error "'constexpr' initializer for a real type is of complex type" } */
constexpr double v99 = __builtin_complex (1.0, 1.0); /* { dg-error "'constexpr' initializer for a real type is of complex type" } */
constexpr double v100 = __builtin_complex (1.0, -0.0); /* { dg-error "'constexpr' initializer for a real type is of complex type" } */
constexpr float v102 = (unsigned long long) -1; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr double v103 = (unsigned long long) -1; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr float v104 = __LONG_LONG_MAX__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr double v105 = __LONG_LONG_MAX__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr signed char v106[] = u8"\xff"; /* { dg-error "'constexpr' initializer not representable in type of object" } */
/* Only the initialized (possibly by default) element of a constexpr union is a
   named constant.  */
union u107 { int a; int b; };
constexpr union u107 v108 = { };
constexpr union u107 v109 = { .a = 0 };
constexpr union u107 v110 = { .b = 0 };
constexpr int v111 = v108.b; /* { dg-error "initializer" } */
constexpr int v112 = v109.b; /* { dg-error "initializer" } */
constexpr int v113 = v110.a; /* { dg-error "initializer" } */
/* A reference to an array in a constexpr object is converted to a pointer as
   usual, so in particular is not equivalent to directly using a string literal
   initializer extracted from the initializer of that object.  */
struct s114 { char c[10]; };
constexpr struct s114 v115 = { "abc" };
constexpr struct s114 v116 = { v115.c }; /* { dg-error "initializer" } */
/* { dg-error "integer from pointer" "conversion" { target *-*-* } .-1 } */

void
f0 ()
{
  (constexpr constexpr int) { 1 }; /* { dg-error "duplicate 'constexpr'" } */
  (constexpr thread_local int) { 1 }; /* { dg-error "'thread_local' used with 'constexpr'" } */
  (thread_local constexpr static int) { 1 }; /* { dg-error "'constexpr' used with '_Thread_local'" } */
  (constexpr int) { sizeof (struct fs1 *) }; /* { dg-error "declared in underspecified object initializer" } */
  (constexpr int) { sizeof (union fs2 *) }; /* { dg-error "declared in underspecified object initializer" } */
  (constexpr int) { sizeof (struct fs3 { int a; }) }; /* { dg-error "defined in underspecified object initializer" } */
  (constexpr int) { sizeof (struct { int a; }) }; /* { dg-error "defined in underspecified object initializer" } */
  (constexpr int) { sizeof (union fs4 { int a; }) }; /* { dg-error "defined in underspecified object initializer" } */
  (constexpr int) { sizeof (union { int a; }) }; /* { dg-error "defined in underspecified object initializer" } */
  (constexpr int) { sizeof (enum fs5 { A }) }; /* { dg-error "defined in underspecified object initializer" } */
  /* The following case is undefined behavior (so doesn't actually require a
     diagnostic).  */
  (constexpr int) { sizeof (enum { B }) }; /* { dg-error "defined in underspecified object initializer" } */
  /* Examples with a forward declaration, then definition inside constexpr.  */
  struct fs6;
  (constexpr int) { sizeof (struct fs6 { int a; }) }; /* { dg-error "defined in underspecified object initializer" } */
  union fs7;
  (constexpr int) { sizeof (union fs7 { int a; }) }; /* { dg-error "defined in underspecified object initializer" } */
  constexpr int fv32 = sizeof (fv32); /* { dg-error "underspecified 'fv32' referenced in its initializer" } */
  /* Test entering then exiting nested underspecified initializers.  */
  constexpr int x = (constexpr int) { 1 } + sizeof (struct fs8 *); /* { dg-error "declared in underspecified object initializer" } */
  auto y = (constexpr int) { 1 } + sizeof (struct fs9 *); /* { dg-error "declared in underspecified object initializer" } */
  extern const int z; /* { dg-message "previous declaration" } */
  constexpr const int z = 1; /* { dg-error "underspecified declaration of 'z', which is already declared in this scope" } */
  /* { dg-error "declaration of 'z' with no linkage follows extern declaration" "linkage error" { target *-*-* } .-1 } */
  int non_const = 1;
  typedef int VLA[non_const];
  constexpr VLA *pnc = nullptr; /* { dg-error "'constexpr' object has variably modified type" } */
  (constexpr t42) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr t43) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr t44) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr volatile double) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr int *restrict) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr _Atomic (short)) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr long *volatile) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr struct t45) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr struct t46) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr struct t47) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr union t58) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr union t59) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr union t60) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr t42 [1][2][3]) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr volatile int [1][2][3]) {}; /* { dg-error "invalid qualifiers for 'constexpr' object" } */
  (constexpr struct t45 [2][2][4]) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr union t60 [2][2][4]) {}; /* { dg-error "invalid qualifiers for field of 'constexpr' object" } */
  (constexpr int) { v68 }; /* { dg-error "initializer element is not constant" } */
  (constexpr double) { exp (0) }; /* { dg-error "initializer element is not a constant expression" } */
  /* { dg-error "'constexpr' initializer is not an arithmetic constant expression" "arithmetic" { target *-*-* } .-1 } */
  (constexpr struct s71) { 0, exp (0) }; /* { dg-error "initializer element is not a constant expression" } */
  /* { dg-error "'constexpr' initializer is not an arithmetic constant expression" "arithmetic" { target *-*-* } .-1 } */
  (constexpr struct s71) { v68, 0 }; /* { dg-error "initializer element is not constant" } */
  (constexpr union u74) { v68 }; /* { dg-error "initializer element is not constant" } */
  (constexpr union u74) { .b = exp (0) }; /* { dg-error "initializer element is not a constant expression" } */
  /* { dg-error "'constexpr' initializer is not an arithmetic constant expression" "arithmetic" { target *-*-* } .-1 } */
  (constexpr struct fs10 *) { 0 }; /* { dg-error "declared in 'constexpr' compound literal" } */
  (constexpr union fs11 *) { 0 }; /* { dg-error "declared in 'constexpr' compound literal" } */
  (constexpr struct fs12 { int a; }) { 0 }; /* { dg-error "defined in 'constexpr' compound literal" } */
  (constexpr union fs13 { int a; }) { 0 }; /* { dg-error "defined in 'constexpr' compound literal" } */
  (constexpr enum fs14 { FS14 }) { FS14 }; /* { dg-error "defined in 'constexpr' compound literal" } */
  (constexpr enum { FS15 }) { FS15 }; /* { dg-error "defined in 'constexpr' compound literal" } */
  (constexpr void *) { (void *) 64 }; /* { dg-error "'constexpr' pointer initializer is not null" } */
  (constexpr int *) { (int *) 64 }; /* { dg-error "'constexpr' pointer initializer is not null" } */
  (constexpr int) { (int) (double) 1.0 }; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */
  (constexpr struct s71) { (int) (double) 1.0, 0 }; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */
  (constexpr struct s93) { (int *) 16 }; /* { dg-error "'constexpr' pointer initializer is not null" } */
  (constexpr int) { (unsigned int) -1 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr unsigned char) { -1 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr signed char) { 1234567LL }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  /* { dg-warning "overflow in conversion" "overflow warning" { target *-*-* } .-1 } */
  (constexpr double) { __builtin_complex (1.0, 0.0) }; /* { dg-error "'constexpr' initializer for a real type is of complex type" } */
  (constexpr double) { __builtin_complex (1.0, 1.0) }; /* { dg-error "'constexpr' initializer for a real type is of complex type" } */
  (constexpr double) { __builtin_complex (1.0, -0.0) }; /* { dg-error "'constexpr' initializer for a real type is of complex type" } */
  (constexpr float) { (unsigned long long) -1 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr double) { (unsigned long long) -1 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr float) { __LONG_LONG_MAX__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr double) { __LONG_LONG_MAX__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr signed char []) { u8"\xff" }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  constexpr typeof (nullptr) not_npc = nullptr;
  int *ptr = 0;
  /* auto may only be used with another storage class specifier, such as
     constexpr, if the type is inferred.  */
  auto constexpr int a_c_t = 1; /* { dg-error "'auto' used with 'constexpr'" } */
  constexpr auto int c_a_t = 1; /* { dg-error "'auto' used with 'constexpr'" } */
  auto int constexpr a_t_c = 1; /* { dg-error "'constexpr' used with 'auto'" } */
  constexpr int auto c_t_a = 1; /* { dg-error "'auto' used with 'constexpr'" } */
  int auto constexpr t_a_c = 1; /* { dg-error "'constexpr' used with 'auto'" } */
  int constexpr auto t_c_a = 1; /* { dg-error "'auto' used with 'constexpr'" } */
}
