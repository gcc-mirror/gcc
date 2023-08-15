/* { dg-do compile }
 * { dg-options "-std=c23" }
 */

// fixed underlying types

enum A : int { N = 1 } x1 = { };
enum B : int { M = 1 } x2 = { };
enum C { U = 1 } x3 = { };

void f(void)
{
	enum A : int { N = 1 } y1 = x1;
	enum B : short { M = 1 } y2;
        y2 = x2;
	enum B : short { M = 1 } y2b;
	enum Bb : short { V = 1 } y2d = x2;
	enum B : short { M = 1 } *y2e = &x2;	/* { dg-error "incompatible" } */
	enum B : short { M = 1 } y2c = x2;
	enum C { U = 1 } y3 = x3;
}

