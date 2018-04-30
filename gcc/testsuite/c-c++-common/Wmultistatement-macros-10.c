/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define SWAP(x, y) \
  tmp = x; /* { dg-warning "macro expands to multiple statements" } */ \
  x = y; \
  y = tmp

#define M1	\
  switch (x) /* { dg-message "not guarded by this 'switch' clause" } */ \
    case 1:	\
    SWAP (x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define M2	\
  switch (x)	\
    case 1:	\
    x++

#define M3	\
  switch (x)	\
    case 1:	\
    x++;;

#define M4	\
  switch (x) /* { dg-message "not guarded by this 'switch' clause" } */ \
L1:		\
    case 1:	\
    SWAP (x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define INC	\
  x++;;

int x, y, tmp;

void
fn0 (void)
{
  switch (x) /* { dg-message "not guarded by this 'switch' clause" } */
    case 1:
      SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */

  switch (x) /* { dg-message "not guarded by this 'switch' clause" } */
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
      SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */
}

void
fn1 (void)
{
  M1; /* { dg-message "in expansion of macro .M1." } */
  M2;
  M3;
  M4; /* { dg-message "in expansion of macro .M4." } */
  goto L1;
}

void
fn2 (void)
{
  switch (x)
    case 1:
      INC

  switch (x)
    case 1:
      ({ x = 10; x++; });
}

void
fn3 (void)
{
  switch (x)
    {
    case 1:
      SWAP (x, y);
    }
}
