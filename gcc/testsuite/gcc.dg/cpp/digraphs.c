/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-std=c99" } */

/* Fully test the 6 digraphs under c99 assumptions.  Four are pasted,
   to check that digraph pasting works.  */

extern int strcmp (const char *, const char *);
extern void abort (void);
extern int puts (const char *);
#define err(str) do { puts(str); abort(); } while (0)

%:define glue(x, y) x %:%: y	/* #define glue(x, y) x ## y. */
#ifndef glue
#error glue not defined!
#endif
%:define str(x) %:x		/* #define str(x) #x */

int main (int argc, char *argv<::>) /* argv[] */
glue (<, %) /* { */
             /* di_str[] = */
  const char di_str glue(<, :)glue(:, >) = str(%:%:<::><%%>%:);

  /* Check the glue macro actually pastes, and that the spelling of
     all digraphs is preserved.  */
  if (glue(str, cmp) (di_str, "%:%:<::><%%>%:"))
    err ("Digraph spelling not preserved!");

  return 0;
glue (%, >) /* } */
