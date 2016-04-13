/* PR c/70436  */
/* { dg-options "-Wparentheses" }  */

int a, b, c;
void bar (void);
void baz (void);

void
foo (void)
{
  int i, j;

  if (a) /* { dg-warning "ambiguous" }  */
    for (;;)
      if (b)
        bar ();
      else
        baz ();

  if (a) /* { dg-warning "ambiguous" }  */
    while (1)
      if (b)
        bar ();
      else
        baz ();

  if (a) /* { dg-warning "ambiguous" }  */
    while (1)
      for (;;)
        if (b)
          bar ();
        else
          baz ();

  if (a) /* { dg-warning "ambiguous" }  */
    while (1)
      while (1)
        if (b)
          bar ();
  else
    baz ();

  if (a) /* { dg-warning "ambiguous" }  */
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        if (b)
          bar ();
  else
    baz ();

  if (a)
    for (i = 0; i < 10; i++)
      if (b) /* { dg-warning "ambiguous" }  */
        for (j = 0; j < 10; j++)
          if (c)
            bar ();
      else
        baz ();

  if (a) /* { dg-warning "ambiguous" }  */
    for (i = 0; i < 10; i++)
      if (b)
        for (j = 0; j < 10; j++)
          if (c)
            bar ();
          else
            baz ();
  else
    bar ();

  if (a) /* { dg-warning "ambiguous" }  */
    for (;;)
      if (b)
        while (1)
          if (a)
            bar ();
          else
            baz ();
      else
        bar ();

  if (a) /* { dg-warning "ambiguous" }  */
    for (;;)
      if (b)
        while (1)
          {
            if (a) { bar (); } else { baz (); }
          }
      else
        bar ();

  if (a)
    for (;;)
      if (b)
        bar ();
      else
        baz ();
  else bar ();

  if (a)
    while (1)
      if (b)
        bar ();
      else
        baz ();
  else bar ();

  if (a)
    for (;;)
      {
        if (b)
          bar ();
        else
          baz ();
      }

  if (a)
    {
      for (;;)
        if (b)
          bar ();
    }
  else baz ();

  if (a)
    do
      if (b) bar (); else baz ();
    while (b);

  if (a)
    do
      if (b) bar ();
    while (b);
  else baz ();
}
