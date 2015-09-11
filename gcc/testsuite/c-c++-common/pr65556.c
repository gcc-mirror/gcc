/* PR c++/65556 */
/* { dg-do compile } */

struct S
{
  long l: 1;
  long l2: 21;
  unsigned long ul: 1;
  unsigned long ul2: 21;
} s;

void
fn ()
{
  switch (s.l)
    case 0:;
  switch (s.ul)
    case 0:;
  switch (s.l2)
    case 0:;
  switch (s.ul2)
    case 0:;
}
