/* Origin: PR c/166 from Joerg Czeranski <jc@joerch.org>.  */
/* In the declaration of proc, x cannot be parsed as a typedef name,
   so it must be parsed as a parameter name.  */
typedef int x;
void proc(int (*x)(void)) {}
