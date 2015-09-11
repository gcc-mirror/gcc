/* PR c/66066 */
/* { dg-do compile } */
/* { dg-options "-Wno-div-by-zero -pedantic-errors" } */

/* Accept these unless -pedantic-errors/-Werror.  */
int a1 = -1 << 0;		/* { dg-error "initializer element is not a constant expression" } */
int a2 = -1 << 0 | 0;		/* { dg-error "initializer element is not a constant expression" } */
int a3 = -1 << 0 & 1;		/* { dg-error "initializer element is not a constant expression" } */
int a4 = -1 << 2 ^ 1;		/* { dg-error "initializer element is not a constant expression" } */
int a5 = 4 & -1 << 2;		/* { dg-error "initializer element is not a constant expression" } */
int a6 = (-1 << 2) ^ (1 >> 1);	/* { dg-error "initializer element is not a constant expression" } */
int a7 = 0 || (-1 << 1);	/* { dg-error "initializer element is not a constant expression" } */
int a8 = 0 ? 2 : (-1 << 1);	/* { dg-error "initializer element is not a constant expression" } */
int a9 = 1 && -1 << 0;		/* { dg-error "initializer element is not a constant expression" } */
int a10 = !(-1 << 0);		/* { dg-error "initializer element is not a constant expression" } */

/* Don't accept these.  */
int b1 = 1 / 0;		/* { dg-error "initializer element is not constant" } */
int b2 = 1 / (1 / 0);	/* { dg-error "initializer element is not constant" } */
int b3 = 0 ? 2 : 1 / 0;	/* { dg-error "initializer element is not constant" } */
int b4 = 0 || 1 / 0;	/* { dg-error "initializer element is not constant" } */
int b5 = 0 * (1 / 0);	/* { dg-error "initializer element is not constant" } */
int b6 = 1 * (1 / 0);	/* { dg-error "initializer element is not constant" } */
int b7 = (1 / 0) * 0;	/* { dg-error "initializer element is not constant" } */
int b8 = (1 / 0) * 1;	/* { dg-error "initializer element is not constant" } */
int b9 = 1 && 1 / 0;	/* { dg-error "initializer element is not constant" } */
int b10 = !(1 / 0);	/* { dg-error "initializer element is not constant" } */
int c1 = 1 % 0;		/* { dg-error "initializer element is not constant" } */
int c2 = 1 / (1 % 0);	/* { dg-error "initializer element is not constant" } */
int c3 = 0 ? 2 : 1 % 0;	/* { dg-error "initializer element is not constant" } */
int c4 = 0 || 1 % 0;	/* { dg-error "initializer element is not constant" } */
int c5 = 0 * (1 % 0);	/* { dg-error "initializer element is not constant" } */
int c6 = 1 * (1 % 0);	/* { dg-error "initializer element is not constant" } */
int c7 = (1 % 0) * 0;	/* { dg-error "initializer element is not constant" } */
int c8 = (1 % 0) * 1;	/* { dg-error "initializer element is not constant" } */
int c9 = 1 && 1 % 0;	/* { dg-error "initializer element is not constant" } */
int c10 = !(1 % 0);	/* { dg-error "initializer element is not constant" } */
