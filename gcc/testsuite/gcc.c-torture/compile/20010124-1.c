/* Origin: testcase from Joseph Myers <jsm28@cam.ac.uk>, problem pointed
   out in a post to comp.std.c
   <980283801.3063.0.nnrp-07.c2deb1c2@news.demon.co.uk>
   by Dibyendu Majumdar <dibyendu@mazumdar.demon.co.uk>.
   Compound literals should be parsed as postfix expressions, rather than
   as cast expressions.  In particular, they are valid operands of sizeof.  */

struct s { int a; int b; };
char x[((sizeof (struct s){ 1, 2 }) == sizeof (struct s)) ? 1 : -1];
