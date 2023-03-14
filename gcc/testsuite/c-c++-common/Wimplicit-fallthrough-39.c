/* PR middle-end/103597 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

#define E(c, e) if (c) e

int
fn0 (int n)
{
  switch (n)
    {
    case 0:
      E (1, return 0);
    case 1:
      return -1;
    }
  return 0;
}

int
fn1 (int n)
{
  switch (n)
    {
    case 0:
      E (1, goto out);
    case 1:
      return -1;
    }
out:
  return 0;
}

int
fn2 (int n)
{
  switch (n)
    {
    case 0:
      if (1)
       	n++;	  /* { dg-warning "statement may fall through" } */
    case 1:	  /* { dg-message "here" } */
      return -1;
    }
  return 0;
}

int
fn3 (int n)
{
  switch (n)
    {
    case 0:
      if (0)		/* { dg-warning "statement may fall through" } */
       	return 0;
    case 1:		/* { dg-message "here" } */
      return -1;
    }
  return 0;
}

int
fn4 (int n)
{
  switch (n)
    {
    case 0:
      E (0, n++);
      --n;	  /* { dg-warning "statement may fall through" } */
    case 1:	  /* { dg-message "here" } */
      return -1;
    }
  return 0;
}

int
fn5 (int n)
{
  switch (n)
    {
    case 0:
      if (1)
	return 0;
      else
	return -1;
    case 1:
      return -1;
    }
  return 0;
}

int
fn6 (int n)
{
  switch (n)
    {
    case 0:
      if (1)
	return 0;
      else
	{
meow:
	  n--;  /* { dg-warning "statement may fall through" } */
	}
    case 1:   /* { dg-message "here" } */
      return -1;
    case 2:
      goto meow;
    }
  return 0;
}

int
fn7 (int n)
{
  switch (n)
    {
    case 0:
      if (1)
       	return 0;
woof:
    case 1:
      return -1;
    }
  return 0;
}

int
fn8 (int n)
{
  switch (n)
    {
    case 0:
      if (1) n++; /* { dg-warning "statement may fall through" } */
woof:		  /* { dg-message "here" } */
    case 1:
      return -1;
    }
  return 0;
}
