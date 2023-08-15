/*
 * { dg-do compile }
 * { dg-options "-std=c23" }
 */

// conflicting types for anonymous structs / unions

extern struct { int x; } a;
extern struct { int x; } a;	/* { dg-error "conflicting types" } */

extern union { int x; } b;
extern union { int x; } b;	/* { dg-error "conflicting types" } */

typedef struct { int x; } u;
typedef struct { int x; } v;

u c;
v c;				/* { dg-error "conflicting types" } */

typedef union { int x; } q;
typedef union { int x; } r;

q d;
r d;				/* { dg-error "conflicting types" } */


