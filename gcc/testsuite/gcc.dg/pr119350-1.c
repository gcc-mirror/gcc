/* PR c/119350 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

struct S { int a; int b[]; };
struct T { struct S c; };			/* { dg-error "invalid use of structure with flexible array member" } */
struct S d = { 1, {} };				/* { dg-error "initialization of a flexible array member" } */
struct S e = { 1, { 2 } };			/* { dg-error "initialization of a flexible array member" } */
struct S f = { .a = 1, .b = {} };		/* { dg-error "initialization of a flexible array member" } */
struct S g = { .a = 1, .b = { 2 } };		/* { dg-error "initialization of a flexible array member" } */
struct T h = { { 1, {} } };			/* { dg-error "initialization of flexible array member in a nested context" } */
struct T i = { { 1, { 2 } } };			/* { dg-error "initialization of flexible array member in a nested context" } */
struct T j = { .c = { .a = 1, .b = {} } };	/* { dg-error "initialization of flexible array member in a nested context" } */
struct T k = { .c = { .a = 1, .b = { 2 } } };	/* { dg-error "initialization of flexible array member in a nested context" } */
