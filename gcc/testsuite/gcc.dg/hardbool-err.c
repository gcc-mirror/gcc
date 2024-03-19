/* { dg-do compile } */
/* { dg-options "" } */

typedef _Bool __attribute__ ((__hardbool__))
hbbl; /* { dg-error "integral types" } */

typedef double __attribute__ ((__hardbool__))
hbdbl; /* { dg-error "integral types" } */

typedef _Complex int __attribute__ ((__hardbool__))
hbcplx; /* { dg-error "integral types" } */

enum x;
typedef enum x __attribute__ ((__hardbool__))
hbenum; /* { dg-error "integral types" } */

struct s;
typedef struct s __attribute__ ((__hardbool__))
hbstruct; /* { dg-error "integral types" } */

typedef int __attribute__ ((__hardbool__ (0, 0)))
hb00; /* { dg-error "different values" } */

typedef int __attribute__ ((__hardbool__ (4, 16))) hb4x;
struct s {
  hb4x m:2;
}; /* { dg-error "is a GCC extension|different values" } */
/* { dg-warning "changes value" "warning" { target *-*-* } .-1 } */

hb4x __attribute__ ((vector_size (4 * sizeof (hb4x))))
vvar; /* { dg-error "invalid vector type" } */
