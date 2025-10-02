/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s1 {
    int (*f)(void);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
};

struct s1 g1;
struct s1 g2;
struct s1 g3;

int (*g_f1)(char);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
