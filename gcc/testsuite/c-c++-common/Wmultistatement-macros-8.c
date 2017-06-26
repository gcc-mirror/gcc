/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define SWAP(x, y) \
  tmp = x; /* { dg-warning "macro expands to multiple statements" } */ \
  x = y; \
  y = tmp

#define M1				\
  for (i = 0; i < 1; ++i) /* { dg-message "not guarded by this 'for' clause" } */ \
    SWAP (x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define M2				\
  for (i = 0; i < 1; ++i)		\
    x++

#define M3				\
  for (i = 0; i < 1; ++i)		\
    x++;;

#define M4				\
  for (i = 0; i < 1; ++i) /* { dg-message "not guarded by this 'for' clause" } */ \
L1:					\
    SWAP (x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define INC	\
  x++;;

int x, y, tmp;

void
fn0 (void)
{
  int i;
  for (i = 0; i < 1; ++i) /* { dg-message "not guarded by this 'for' clause" } */
    SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */

  for (i = 0; i < 1; ++i) /* { dg-message "not guarded by this 'for' clause" } */
L:
    SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */
  goto L;
}

void
fn1 (void)
{
  int i;
  M1; /* { dg-message "in expansion of macro .M1." } */
  M2;
  M3;
  M4; /* { dg-message "in expansion of macro .M4." } */
  goto L1;
}

void
fn2 (void)
{
  for (int i = 0; i < 1; ++i)
    INC

  for (int i = 0; i < 1; ++i)
    ({ x = 10; x++; });
}
