/*
 * { dg-do compile }
 * { dg-options "-std=gnu23" }
 */

struct r { int a; char b[]; };
struct r { int a; char b[0]; };	/* allowed GNU extension */
struct r { int a; char b[1]; }; /* { dg-error "redefinition of struct or union" } */


