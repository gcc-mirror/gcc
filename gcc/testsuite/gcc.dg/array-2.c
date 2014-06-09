/* { dg-do compile } */
/* { dg-options "" } */

/* Verify that we can't do things to get ourselves in trouble
   with GCC's initialized flexible array member extension.  */

struct f { int w; int x[]; };
struct g { struct f f; };
struct g g1 = { { 0, { } } };
struct g g2 = { { 0, { 1 } } }; /* { dg-error "nested context" "nested" } */
				/* { dg-message "near init" "near" { target *-*-* } 10 } */
struct h { int x[0]; int y; };
struct h h1 = { { 0 }, 1 }; /* { dg-warning "excess elements" "excess" } */
			    /* { dg-message "near init" "before end" { target *-*-* } 13 } */
