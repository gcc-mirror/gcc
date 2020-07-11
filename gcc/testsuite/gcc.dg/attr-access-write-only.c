/* PR middle-end/83859 - attribute to establish relation between parameters
   for buffer and its size
   Test to verify the handling of attribute access (write_only) syntax.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

int  __attribute__ ((access))
access_v (void);       // { dg-error "wrong number of arguments specified for 'access' attribute" }

int  __attribute__ ((access ()))
access___v (void);     // { dg-error "wrong number of arguments specified for 'access' attribute" }

int  __attribute__ ((access (wronly)))
wronly_spelling (void);   // { dg-error "attribute .access. invalid mode 'wronly'; expected one of 'read_only', 'read_write', 'write_only', or 'none'" }

int  __attribute__ ((access (read_only)))
wronly_v_all (void);   // { dg-error "attribute .access\\(read_only\\). missing an argument" }

int  __attribute__ ((access (read_only ())))
wronly___v_all (void);   // { dg-error "attribute 'access' unexpected '\\(' after mode 'read_only'; expected a positional argument or '\\)'" }
// { dg-warning "implicit declaration of function 'read_only'" "" { target *-*-* } .-2 }


int wronly (void);

int  __attribute__ ((access (wronly ())))
wronly___v_all (void);   // { dg-error "attribute 'access' invalid mode 'wronly'" }

#define WRONLY(...)  __attribute__ ((access (write_only, __VA_ARGS__)))

int WRONLY (1)
wronly_pcv_1 (void*);
int WRONLY (2)
wronly_i_pcv_2 (int, void*);
int WRONLY (3)
wronly_i_i_pcv_3 (int, int, void*);

int WRONLY (0 + 1)
wronly_pcv_0p1 (void*);

int WRONLY (2 - 1)
wronly_pcv_2m1 (void*);

int WRONLY (1, 1)
wronly_pv_pi_1_1 (void*, const int*);      // { dg-error "attribute 'access\\(write_only, 1, 1\\)' positional argument 2 references non-integer argument type 'void \\*'" }

int WRONLY (1, 2)
wronly_pcv_pc_1_2 (void*, char*);   // { dg-error "attribute .access\\(write_only, 1, 2\\)' positional argument 2 references non-integer argument type 'char \\*'" }

int WRONLY (2, 1)
wronly_pcd_pcv_2_1 (const double*, void*);   // { dg-error "attribute .access\\(write_only, 2, 1\\)' positional argument 2 references non-integer argument type 'const double \\*'" }

int WRONLY (2, 2)
wronly_pi_pcv_2_2 (int*, void*);   // { dg-error "positional argument 2 references non-integer argument type 'void \\*'" }

int WRONLY (4)
wronly_i_i_i_4 (int, int, int);   // { dg-error "attribute 'access\\(write_only, 4\\)' positional argument 1 value 4 exceeds number of function arguments 3" }

int WRONLY (1)
wronly_i_1 (int);   // { dg-error "attribute 'access\\(write_only, 1\\)' positional argument 1 references non-pointer argument type 'int'" }

int WRONLY (2)
wronly_i_pc (int, const char*);   // { dg-error "attribute 'access\\(write_only, 2\\)' positional argument 1 references 'const'-qualified argument type 'const char \\*'" }

int WRONLY (-1)
wronly_pcv_m1 (void*);   // { dg-error "attribute 'access\\(write_only, -1\\)' positional argument 1 invalid value -1" }

int WRONLY (1, -12345)
wronly_pcv_i_1_m12345 (void*, int*);   // { dg-error "attribute 'access\\(write_only, 1, -12345\\)' positional argument 2 invalid value -12345" }

int WRONLY ("blah")
wronly_pcv_str (void*);   // { dg-error "attribute 'access\\(write_only, \"blah\"\\)' invalid positional argument 1" }

int WRONLY (1, "foobar")
wronly_pcv_i_1_str (void*, int);   // { dg-error "attribute 'access\\(write_only, 1, \"foobar\"\\)' invalid positional argument 2" }

// Verify that attributes whose operands reference function pointers
// are rejected.
typedef int F (int, int);
WRONLY (1) void wronly_pf_1 (F*);   // { dg-error "attribute 'access\\(write_only, 1\\)' positional argument 1 references argument of function type 'F' \\{aka 'int\\(int,  *int\\)'\\}" }

// Verify pointers to functions.
void WRONLY(2) (*pwronly_pcv2)(int, void*);
void WRONLY(3, 1) (*pwronly_pcv2_1)(int, void*, void*);
void WRONLY(1, 2) (*pwronly_i_pcv_1_2)(int, void*);   // { dg-error "attribute 'access\\(write_only, 1, 2\\)' positional argument 1 references non-pointer argument type 'int'" }

// Verify types.
typedef WRONLY (2) void wronly_p2_t (const int*, char*, const void*);
typedef WRONLY (2) void wronly_p2_1 (int, int*);
