/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define SWAP(X, Y)	\
  tmp = X; /* { dg-warning "macro expands to multiple statements" } */ \
  X = Y;		\
  Y = tmp

#define STUFF		\
  if (0) x = y

#define STUFF2		\
  if (0) x = y; x++

#define STUFF3		\
  if (x) /* { dg-message "not guarded by this 'if' clause" } */ \
    SWAP(x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define SET(X, Y)	\
  (X) = (Y)

#define STUFF4		\
  if (x)		\
    SET(x, y);		\
  SET(x, y)

#define STUFF5		\
  { tmp = x; x = y; }

#define STUFF6		\
  x++;;

int x, y, tmp;

void
fn1 (void)
{
  if (x) /* { dg-message "not guarded by this 'if' clause" } */
    SWAP(x, y); /* { dg-message "in expansion of macro .SWAP." } */
}

void
fn2 (void)
{
  SWAP(x, y);
}

void
fn3 (void)
{
  if (x)
    {
      SWAP(x, y);
    }
}

void
fn4 (void)
{
  if (x)
  ({ x = 10; x++; });
}

void
fn5 (void)
{
  if (x) /* { dg-message "not guarded by this 'if' clause" } */
L1:
    SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */
  goto L1;
}

void
fn6 (void)
{
  if (x)
    SET (x, y);
  SET (tmp, x);
}

void
fn7 (void)
{
  STUFF;
}

void
fn8 (void)
{
  STUFF2;
}

void
fn9 (void)
{
  STUFF3; /* { dg-message "in expansion of macro .STUFF3." } */
}

void
fn10 (void)
{
  STUFF4;
}

void
fn11 (void)
{
  if (x)
    STUFF5;
}

void
fn12 (void)
{
  if (x)
    STUFF6;
}
