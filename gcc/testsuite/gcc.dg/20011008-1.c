/* { dg-do compile } */
/* { dg-options "-O0 -fshow-column" } */

struct { int; int q; } a; /* { dg-warning "13:does not declare anything" } */
struct { union {int x;}; int q; } b;
struct { struct {int x;}; int q; } c;
union { union {int x;}; int q; } d;
union { struct {int x;}; int q; } e;
