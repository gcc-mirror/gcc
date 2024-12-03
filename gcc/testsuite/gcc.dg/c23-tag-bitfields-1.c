/* { dg-do compile } */
/* { dg-options "-std=c23" } */
/* { dg-require-effective-target int32plus } */

struct bar0 { int r : 16; };
struct bar0 { int r : 16; };

struct bar1 { int r : 16; };
struct bar1 { int r : 17; };			/* { dg-error "redefinition" } */

extern struct { int r : 14; } a;
extern struct { int r : 14; } b;

extern struct { int r : 14; } x;
extern struct { int r : 13; } x;		/* { dg-error "conflicting" } */

struct bar2 { int s; int: 0; };
struct bar2 { int s; int: 15; };		/* { dg-error "redefinition" } */

struct bar3 { int s; int : 0; };
struct bar3 { int s; };				/* { dg-error "redefinition" } */

struct bar4 { int r : 3; };
struct bar4 { const int r : 3; };		/* { dg-error "redefinition" } */

struct bar5 { int r : 16; };
struct bar5 { int r; };				/* { dg-error "redefinition" } */

union ubar { int r : 16; };
union ubar { int r : 16; };

union ubar1 { int r : 16; };
union ubar1 { int r : 17; };			/* { dg-error "redefinition" } */

extern union { int r : 14; } c;
extern union { int r : 14; } d;

extern union { int r : 14; } y;
extern union { int r : 13; } y;			/* { dg-error "conflicting" } */

union ubar2 { int s; int : 0; };
union ubar2 { int s; int : 15; };		/* { dg-error "redefinition" } */

union ubar3 { int s: 3; int: 0; };
union ubar3 { int s: 3; };			/* { dg-error "redefinition" } */

union ubar4 { int r : 3; };
union ubar4 { const int r : 3; };		/* { dg-error "redefinition" } */

union ubar5 { int r : 16; };
union ubar5 { int r; };				/* { dg-error "redefinition" } */

