/* PR c/119350 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

struct S { int a; int b[]; };
struct T { struct S c; };
struct S d = { 1, {} };				/* { dg-warning "ISO C forbids empty initializer braces before C23" } */
struct S e = { 1, { 2 } };
struct S f = { .a = 1, .b = {} };		/* { dg-warning "ISO C forbids empty initializer braces before C23" } */
struct S g = { .a = 1, .b = { 2 } };
struct T h = { { 1, {} } };			/* { dg-warning "ISO C forbids empty initializer braces before C23" } */
struct T i = { { 1, { 2 } } };			/* { dg-error "initialization of flexible array member in a nested context" } */
struct T j = { .c = { .a = 1, .b = {} } };	/* { dg-warning "ISO C forbids empty initializer braces before C23" } */
struct T k = { .c = { .a = 1, .b = { 2 } } };	/* { dg-error "initialization of flexible array member in a nested context" } */
