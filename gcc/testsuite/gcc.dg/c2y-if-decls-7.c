/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wall -Wextra" } */
/* Test VLAs.  Invalid code.  */

void foo (int) { }

void
g ()
{
  int i = 3;

  switch (i)  /* { dg-message "switch starts here" } */
    {  /* { dg-warning "statement will never be executed" } */
      int arr[i] = { };
    default:	  /* { dg-error "switch jumps into scope" } */
      i = arr[0];
      break;
    }
}

void
g2 (int i)
{
  goto L1;  /* { dg-error "jump into scope" } */
  if (int arr[i] = { }; arr[0])
    {
L1:
    }

  goto L2;  /* { dg-error "jump into scope" } */
  if (int arr[i] = { })  /* { dg-warning "will always evaluate as .true." } */
    {
L2:
      arr[0] = 42;
    }

  goto L3;  /* { dg-error "jump into scope" } */
  switch (int arr[i] = { }; arr[0])
    {
    case 0:
    L3:
      return;
    }
}

void
g3 (int i, int n)
{
  switch (i)
    {
    case 0:
      if (int arr[n] = { })  /* { dg-warning "will always evaluate as .true." } */
	{
    case 3:  /* { dg-error "switch jumps into scope" } */
	  arr[0] = 42;
	}
      break;
    default:
      return;
    }
}

void
g4 (int i, int n)
{
  switch (i)
    {
    case 0:
      if (int arr[n] = { }; arr[0])
	{
    case 3:  /* { dg-error "switch jumps into scope" } */
	}
      break;
    default:
      return;
    }
}

void
g5 (int i, int n)
{
  switch (i)
    {
    case 0:
      goto L;  /* { dg-error "jump into scope" } */
      switch (int arr[n] = { }; arr[0])
	{
    L:
	}
      break;
    default:
      return;
    }
}
