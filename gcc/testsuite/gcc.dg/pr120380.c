/* PR c/120380 */
/* { dg-do compile } */

struct pair_t {
  char c;
  int i;
};
typedef struct foo_ {			/* { dg-error "no member" } */
  struct foo_ {				/* { dg-error "nested redefinition" } */
	  				/* { dg-error "no member" "" { target *-*-* } .-1 } */
    struct foo_ {			/* { dg-error "nested redefinition" } */
      int value;
    }
  }					/* { dg-error "does not declare anything" } */
					/* { dg-error "no semicolon" "" { target *-*-* } .-1 } */
} __attribute__((packed)) foo;		/* { dg-error "does not declare anything" } */
					/* { dg-error "no semicolon" "" { target *-*-* } .-1 } */
struct pair_t p = {0, 1};
foo *addr = (foo *)&p.i;
int main() {
  addr->value = 0;			/* { dg-error "has no member" } */
  return 0;
}

