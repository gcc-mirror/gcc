/* { dg-do compile } */
/* { dg-options "-O0" } */

struct { int; int q; } a; /* { dg-error "unnamed" } */
struct { union {int x;}; int q; } b;
struct { struct {int x;}; int q; } c;
union { union {int x;}; int q; } d;
union { struct {int x;}; int q; } e;
