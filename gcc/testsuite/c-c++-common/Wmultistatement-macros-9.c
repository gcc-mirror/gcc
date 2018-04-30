/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define SWAP(x, y) \
  tmp = x; /* { dg-warning "macro expands to multiple statements" } */ \
  x = y; \
  y = tmp

#define M1	\
  while (x) /* { dg-message "not guarded by this 'while' claus" } */ \
    SWAP (x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define M2	\
  while (x)	\
    x++

#define M3	\
  while (x)	\
    x++;;

#define M4	\
  while (x) /* { dg-message "not guarded by this 'while' claus" } */ \
L1:		\
    SWAP (x, y) /* { dg-message "in expansion of macro .SWAP." } */

#define INC	\
  x++;;

int x, y, tmp;

void
fn0 (void)
{
  while (x) /* { dg-message "not guarded by this 'while' claus" } */
    SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */

  while (x) /* { dg-message "not guarded by this 'while' claus" } */
L:
    SWAP (x, y); /* { dg-message "in expansion of macro .SWAP." } */
  goto L;
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
  while (x)
    INC

  while (x)
    ({ x = 10; x++; });
}
