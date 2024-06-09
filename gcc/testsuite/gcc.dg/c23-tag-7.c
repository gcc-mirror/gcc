/*
 * { dg-do compile }
 * { dg-options "-std=c23" }
 */

// recursive declarations

extern struct bar { struct bar* p; int x; } b;
extern struct bar { struct bar* p; int x; } b;

struct foo { struct foo { struct foo* p; int x; }* p; int x; } a;	/* { dg-error "nested" } */

