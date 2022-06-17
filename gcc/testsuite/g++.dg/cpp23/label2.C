// P2324R2 - Labels at the end of compound statements
// PR c++/103539
// { dg-do compile { target c++23 } }
// Test bad cases.

void
fn1 ()
{
  /* A selection-statement wants a statement, but a mere label isn't a statement.  */
  if (1)
lab:
} // { dg-error "expected" }

void
fn2 ()
{
  if (0)
    {
    }
  else
lab:
} // { dg-error "expected" }

void
fn3 ()
{
  do
lab:
  while (0); // { dg-error "expected" }
} // { dg-error "expected" }

void
fn4 ()
{
  for (;;)
lab:
} // { dg-error "expected" }

void
fn5 ()
{
  switch (1)
  lab:
} // { dg-error "expected" }

void
fn6 ()
{
  if (1)
lab1:
lab2:
} // { dg-error "expected" }
