/* ICE with flexible arrays in non-lvalue structures.  Bug 16566
   (testcase from duplicate bug 16575).  */

struct S;
struct C {
    int i;
    struct S *tab[];
};
struct S { struct C c; };
void foo (struct S *x) {
  ((void)1, x->c).tab[0] = 0;
}
