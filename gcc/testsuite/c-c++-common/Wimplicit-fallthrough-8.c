/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void grace (int);

int
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

int
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

int
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

int
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

int
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

int
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
