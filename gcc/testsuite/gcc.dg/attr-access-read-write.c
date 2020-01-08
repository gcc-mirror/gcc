/* PR middle-end/83859 - attribute to establish relation between parameters
   for buffer and its size
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

int  __attribute__ ((access))
access_v (void);       /* { dg-error "wrong number of arguments specified for 'access' attribute" } */

int  __attribute__ ((access ()))
access___v (void);     /* { dg-error "wrong number of arguments specified for 'access' attribute" } */

int  __attribute__ ((access (rdwr)))
rdwr_spelling (void);   /* { dg-error "attribute .access. invalid mode 'rdwr'; expected one of 'read_only', 'read_write', or 'write_only'" } */

int  __attribute__ ((access (read_write)))
rdwr_v_all (void);   /* { dg-error "attribute .access\\(read_write\\). missing an argument" } */

int  __attribute__ ((access (read_write ())))
rdwr___v_all (void);   /* { dg-error "attribute 'access' unexpected '\\(' after mode 'read_write'; expected a positional argument or '\\)'" } */
/* { dg-warning "implicit declaration of function 'read_write'" "" { target *-*-* } .-2 } */


int rdwr (void);

int  __attribute__ ((access (rdwr ())))
rdwr___v_all (void);   /* { dg-error "attribute 'access' invalid mode 'rdwr'" } */


#define RDWR(...)  __attribute__ ((access (read_write, __VA_ARGS__)))

int RDWR (1)
rdwr_pcv_1 (void*);

int RDWR (2)
rdwr_i_pcv_2 (int, void*);
int RDWR (3)
rdwr_i_i_pcv_3 (int, int, void*);

int RDWR (0 + 1)
rdwr_pcv_0p1 (void*);

int RDWR (2 - 1)
rdwr_pcv_2m1 (void*);

int RDWR (1)
rdwr_pcv_pi_1_1 (const void*, int*);    /* { dg-error "attribute 'access\\(read_write, 1\\)' positional argument 1 references 'const'-qualified argument type 'const void \\*'" } */

int RDWR (1, 1)
rdwr_pv_pi_1_1 (void*, int*);      /* { dg-error "attribute 'access\\(read_write, 1, 1\\)' positional argument 2 references non-integer argument type 'void \\*'" } */

int RDWR (1, 2)
rdwr_pcv_pc_1_2 (void*, char*);   /* { dg-error "attribute .access\\(read_write, 1, 2\\)' positional argument 2 references non-integer argument type 'char \\*'" } */

int RDWR (2, 1)
rdwr_pcd_pcv_2_1 (double*, void*);   /* { dg-error "attribute .access\\(read_write, 2, 1\\)' positional argument 2 references non-integer argument type 'double \\*'" } */

int RDWR (2, 2)
rdwr_pi_pcv_2_2 (int*, void*);   /* { dg-error "positional argument 2 references non-integer argument type 'void \\*'" } */

int RDWR (4)
rdwr_i_i_i_4 (int, int, int);   /* { dg-error "attribute 'access\\(read_write, 4\\)' positional argument 1 value 4 exceeds number of function arguments 3" } */

int RDWR (1)
rdwr_i_1 (int);   /* { dg-error "attribute 'access\\(read_write, 1\\)' positional argument 1 references non-pointer argument type 'int'" } */

int RDWR (2)
rdwr_i_pc (int, const char*);   /* { dg-error "attribute 'access\\(read_write, 2\\)' positional argument 1 references 'const'-qualified argument type 'const char \\*'" } */

int RDWR (-1)
rdwr_pcv_m1 (void*);   /* { dg-error "attribute 'access\\(read_write, -1\\)' positional argument 1 invalid value -1" } */

int RDWR (1, -12345)
rdwr_pcv_i_1_m12345 (void*, int*);   /* { dg-error "attribute 'access\\(read_write, 1, -12345\\)' positional argument 2 invalid value -12345" } */

int RDWR ("blah")
rdwr_pcv_str (void*);   /* { dg-error "attribute 'access\\(read_write, \"blah\"\\)' invalid positional argument 1" } */

int RDWR (1, "foobar")
rdwr_pcv_i_1_str (void*, int);   /* { dg-error "attribute 'access\\(read_write, 1, \"foobar\"\\)' invalid positional argument 2" } */

/* Verify that attributes whose operands reference function pointers
   are rejected.  */
typedef int F (int, int);
RDWR (1) void rdwr_pf_1 (F*);   /* { dg-error "attribute 'access\\(read_write, 1\\)' positional argument 1 references argument of function type 'F' \\{aka 'int\\(int,  *int\\)'\\}" } */

/* Verify pointers to functions.  */
void RDWR(2) (*prdwr_pv2)(int, void*);
void RDWR(3, 1) (*prdwr_pv2_1)(int, void*, void*);

/* Verify types.  */
typedef RDWR (2) void rdwr_p2_t (int*, char*, void*);
typedef RDWR (2) void rdwr_p2_1 (int, int*);
