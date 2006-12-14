// { dg-do compile }
// { dg-options "-Wparentheses" }

// C++ version of gcc.dg/Wparentheses-9.c

int foo (int);

int a, b, c;

int
bar (void)
{
  if (a)
    foo (0);
  if (b)
    foo (1);
  else
    foo (2);
  if (c) // { dg-warning "ambiguous" "correct warning" }
    if (a)
      foo (3);
    else
      foo (4);
  if (a)
    if (c)
      foo (5);
  if (a)
    if (b) // { dg-warning "ambiguous" "correct warning" }
      if (c)
	foo (6);
      else
	foo (7);
  if (a) // { dg-warning "ambiguous" "correct warning" }
    if (b)
      if (c)
	foo (8);
      else
	foo (9);
    else
      foo (10);
  if (a)
    if (b)
      if (c)
	foo (11);
      else
	foo (12);
    else
      foo (13);
  else
    foo (14);
  if (a) {
    if (b)
      if (c)
	foo (15);
      else
	foo (16);
    else
      foo (17);
  }
}
