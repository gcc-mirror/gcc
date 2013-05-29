/* { dg-do compile } */

/* The ISO C++ standard says, in Section 3.3.2 sentence 4, that a name
   declared in the for-init-statement or in the condition of an if, for
   while, or switch statement can't be redeclared in the outermost block
   of the controlled statement.  (Note, this is not an error in C.)  */

extern void foo (int);
extern int j;

void
e0 (void)
{
  for (int i = 0;	// { dg-message "previously declared here" "prev" }
       i < 10; ++i)
    {
      int i = 2;	// { dg-error "redeclaration" "redecl" }
      foo (i);
    }
}

void
e1 (void)
{
  int i;
  for (i = 0;
       int k = j; i++)	// { dg-message "previously declared here" "prev" }
    {
      int k = 2;	// { dg-error "redeclaration" "redecl" }
      foo (k);
    }
}

void
e2 (void)
{
  if (int i = 1)	// { dg-message "previously declared here" "prev" }
    {
      int i = 2;	// { dg-error "redeclaration" "redecl" }
      foo (i);
    }
}

void
e3 (void)
{
  if (int i = 1)	// { dg-message "previously declared here" "prev" }
    {
      foo (i);
    }
  else
    {
      int i = 2;	// { dg-error "redeclaration" "redecl" }
      foo (i);
    }
}

void
e4 (void)
{
  while (int i = 1)	// { dg-message "previously declared here" "prev" }
    {
      int i = 2;	// { dg-error "redeclaration" "redecl" }
      foo (i);
    }
}

void
e5 (void)
{
  switch (int i = j)	// { dg-message "previously declared here" "prev" }
    {
    int i;		// { dg-error "redeclaration" "redecl" }
    default:
      {
        i = 2;
        foo (i);
      }
    }
}

void
f0 (void)
{
  for (int i = 0; i < 10; ++i)
    {
      foo (i);
      {
        int i = 2;	// OK, not outermost block.
        foo (i);
      }
    }
}

void
f1 (void)
{
  int i;
  for (i = 0; int k = j; i++)
    {
      foo (k);
      {
	int k = 2;	// OK, not outermost block.
	foo (k);
      }
    }
}

void
f2 (void)
{
  if (int i = 1)
    {
      foo (i);
      {
	int i = 2;	// OK, not outermost block.
	foo (i);
      }
    }
}

void
f3 (void)
{
  if (int i = 1)
    {
      foo (i);
    }
  else
    {
      foo (i+2);
      {
	int i = 2;	// OK, not outermost block.
	foo (i);
      }
    }
}

void
f4 (void)
{
  while (int i = 1)
    {
      foo (i);
      {
	int i = 2;	// OK, not outermost block.
	foo (i);
      }
    }
}

void
f5 (void)
{
  switch (int i = j)
    {
    default:
      {
        int i = 2;	// OK, not outermost block.
        foo (i);
      }
    }
}

void
f6 (void)
{
  int i = 1;

  for (int j = 0; j < 10; j++)
    {
      int i = 2;	// OK, not variable from for-init.
      foo (i);
    }
}
