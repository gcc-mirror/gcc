/* PR17544 Incorrect -Wunreachable-code warning
   Origin: sebor@roguewave.com

   G++ appends a "return 0;" when finishing a function, but it was not
   given a source location.  The gimplifier thinks a return statement
   needs a locus so it would add one, making the compiler generated code
   visible to the unreachable code warning.  */

/* { dg-do compile } */
/* { dg-options "-O -Wunreachable-code" } */

int
main (int argc, char *argv[])
{
  const char* const s = argc < 2 ? "" : argv [1];
  int i = 0;
  do {
    ++i;
  } while (i < s [0]);
  return i;
}

