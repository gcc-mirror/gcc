/* ICE with flexible arrays in non-lvalue structures.  Bug 16566
   (comment #3).  */

struct S;

struct C {
    int i;
    struct S *tab[];
};

struct S { struct C c; };

void foo (struct S *x) {
  foo(((void)1, x->c).tab[0]);
}
