/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void grace (int);

void
fn1 (int i)
{
  switch (i)
    case 1:
    if (i == 5)
      grace (0);
    else
      goto done;
done:;
}

void
fn2 (int i)
{
  switch (i)
    {
    case 1:
      if (i == 5) /* { dg-warning "statement may fall through" } */
	grace (0);
      else
	goto done;
    case 2:
      --i;
    }
done:;
}

void
fn3 (int i)
{
  switch (i)
    {
    case 1:
    if (i == 5)
      goto done;
    else
      goto done;
    }
done:;
}

void
fn4 (int i)
{
  switch (i)
    {
    case 1:
      if (i == 5)
	{
	  grace (1);
	  goto done;
	}
      else
	goto done;
    case 2:;
    }
done:;
}

void
fn5 (int i)
{
  switch (i)
    {
    case 1:
      if (i == 5)
	{
	  grace (1);
	  goto done;
	}
      else
	grace (4); /* { dg-warning "statement may fall through" } */
    case 2:
      grace (9);
    }
done:;
}

void
fn6 (int i)
{
  switch (i)
    {
    case 1:
      if (i == 5) /* { dg-warning "statement may fall through" } */
	{
	  grace (1);
	  goto done;
	}
    case 2:
      grace (8);
    }
done:;
}
