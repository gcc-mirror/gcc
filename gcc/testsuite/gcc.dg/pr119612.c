/* PR c/119612
 * { dg-do compile }
 * { dg-options "-std=gnu23" }
 * */

int n = 3;
void a(struct { char (*p)[n]; } *);	/* { dg-warning "anonymous struct" } */
void b(struct { char (*p)[n]; } *);	/* { dg-warning "anonymous struct" } */

