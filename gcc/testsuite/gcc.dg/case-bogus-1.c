/* { dg-do compile } */

void
foo (int n)
{
  switch (n)
    case 0: case 3: case 0.2: case 5:; /* { dg-error "21:case label does not reduce to an integer constant" } */
}
