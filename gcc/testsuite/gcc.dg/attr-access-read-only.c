/* PR middle-end/83859 - attribute to establish relation between parameters
   for buffer and its size
   Test to verify the handling of attribute access (read_only) syntax.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

int  __attribute__ ((access))
access_v (void);       // { dg-error "wrong number of arguments specified for 'access' attribute" }

int  __attribute__ ((access ()))
access___v (void);     // { dg-error "wrong number of arguments specified for 'access' attribute" }

int  __attribute__ ((access (rdonly)))
rdonly_spelling (void);   // { dg-error "attribute .access. invalid mode 'rdonly'; expected one of 'read_only', 'read_write', 'write_only', or 'none'" }

int  __attribute__ ((access (read_only)))
rdonly_v_all (void);   // { dg-error "attribute .access\\(read_only\\). missing an argument" }

int  __attribute__ ((access (read_only ())))
rdonly___v_all (void);   // { dg-error "attribute 'access' unexpected '\\(' after mode 'read_only'; expected a positional argument or '\\)'" }
// { dg-warning "implicit declaration of function 'read_only'" "" { target *-*-* } .-2 }


int rdonly (void);

int  __attribute__ ((access (rdonly ())))
rdonly___v_all (void);   // { dg-error "attribute 'access' invalid mode 'rdonly'" }


int  __attribute__ ((access (read_only)))
rdonly_i_all (int);   // { dg-error "attribute .access\\(read_only\\). missing an argument" }

#define rdonly       __attribute__ ((access (read_only)))
#define RDONLY(...)  __attribute__ ((access (read_only, __VA_ARGS__)))

int RDONLY (1)
rdonly_pcv_1 (const void*);
int RDONLY (2)
rdonly_i_pcv_2 (int, const void*);
int RDONLY (3)
rdonly_i_i_pcv_3 (int, int, const void*);

int RDONLY (0 + 1)
rdonly_pcv_0p1 (const void*);

int RDONLY (2 - 1)
rdonly_pcv_2m1 (const void*);

int RDONLY (1, 1)
rdonly_pv_pi_1_1 (const void*, const int*);      // { dg-error "attribute 'access\\(read_only, 1, 1\\)' positional argument 2 references non-integer argument type 'const void \\*'" }

int RDONLY (1, 2)
rdonly_pcv_pc_1_2 (const void*, char*);   // { dg-error "attribute .access\\(read_only, 1, 2\\)' positional argument 2 references non-integer argument type 'char \\*'" }

int RDONLY (2, 1)
rdonly_pcd_pcv_2_1 (const double*, const void*);   // { dg-error "attribute .access\\(read_only, 2, 1\\)' positional argument 2 references non-integer argument type 'const double \\*'" }

int RDONLY (2, 2)
rdonly_pi_pcv_2_2 (int*, const void*);   // { dg-error "positional argument 2 references non-integer argument type 'const void \\*'" }

int RDONLY (4)
rdonly_i_i_i_4 (int, int, int);   // { dg-error "attribute 'access\\(read_only, 4\\)' positional argument 1 value 4 exceeds number of function arguments 3" }

int RDONLY (1)
rdonly_i_1 (int);   // { dg-error "attribute 'access\\(read_only, 1\\)' positional argument 1 references non-pointer argument type 'int'" }

// It's okay if the pointer argument is non-const, although a separate
// warning encouraging one might be worthwhile.  Maybe something like
// -Wsuggest-const.
int RDONLY (2)
rdonly_i_pc (int, char*);

int RDONLY (-1)
rdonly_pcv_m1 (const void*);   // { dg-error "attribute 'access\\(read_only, -1\\)' positional argument 1 invalid value -1" }

int RDONLY (1, -12345)
rdonly_pcv_i_1_m12345 (const void*, int*);   // { dg-error "attribute 'access\\(read_only, 1, -12345\\)' positional argument 2 invalid value -12345" }

int RDONLY ("blah")
rdonly_pcv_str (const void*);   // { dg-error "attribute 'access\\(read_only, \"blah\"\\)' invalid positional argument 1" }

int RDONLY (1, "foobar")
rdonly_pcv_i_1_str (const void*, int);   // { dg-error "attribute 'access\\(read_only, 1, \"foobar\"\\)' invalid positional argument 2" }

// Verify that attributes whose operands reference function pointers
// are rejected.
typedef int F (int, int);
RDONLY (1) void rdwr_pf_1 (F*);   // { dg-error "attribute 'access\\(read_only, 1\\)' positional argument 1 references argument of function type 'F' \\{aka 'int\\(int,  *int\\)'\\}" }

// Verify pointers to functions.
void RDONLY(2) (*prdonly_pcv2)(int, const void*);
void RDONLY(3, 1) (*prdonly_pcv2_1)(int, void*, const void*);

// Verify types.
typedef RDONLY (2) void rdonly_p2_t (const int*, const char*, const void*);
typedef RDONLY (2) void rdonly_p2_1 (int, const int*);
