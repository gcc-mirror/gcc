/* A redefinition of an empty struct should be diagnosed the same as a
   redefinition of any other tag, but formerly only s2 and s4 were
   diagnosed.  Bug 17188.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s0 { };
struct s0;
struct s0 { }; /* { dg-error "error: redefinition of `struct s0'" } */

struct s1 { };
struct s1 { }; /* { dg-error "error: redefinition of `struct s1'" } */

struct s2 { int a : 1; };
struct s2 { int a : 1; }; /* { dg-error "error: redefinition of `struct s2'" } */

struct s3 { };
struct s3 { int a : 1; }; /* { dg-error "error: redefinition of `struct s3'" } */

struct s4 { int a : 1; };
struct s4 { }; /* { dg-error "error: redefinition of `struct s4'" } */

struct s5 { int a : 1; };
struct s5;

struct s6;
struct s6 { int a : 1; };

struct s7;
struct s7 { };
