/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s1 {
    int (*f)(void);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
};

struct s2 {
    struct s1 *s;
    int a;
};

struct s2 g_s2;
