/* Test obsolete forms of designated initializers.  Test with default
   warning options: valid forms are accepted, while ".member" without
   "=" should not be (bug 16667).  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */
struct s { int a; };
struct s s0 = { .a = 1 };
struct s s1 = { a: 1 };

int x0[] = { [0] = 1 };
int x1[] = { [0] 1 };

/* Invalid syntax: multiple designators without "=".  */
int x2[2][2] = { [0][0] 1 }; /* { dg-error "syntax error|parse error|expected" } */


/* Invalid syntax: C99-style structure designator without "=".  */
struct s s2 = { .a 1 }; /* { dg-error "syntax error|parse error|expected" } */
