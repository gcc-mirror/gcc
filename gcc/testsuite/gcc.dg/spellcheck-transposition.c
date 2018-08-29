/* PR other/69968.  */

struct {
  int coordx, coordy, coordz;
  int coordx1, coordy1, coordz1;
} c;

/* Consider the misspelling "coorzd1".

   With Levenshtein distance, the misspelling has an edit distance of 2
   to all 6 of the fields (e.g. via a deletion and a substitution for the
   first three, and via deletion and insertion for the second three).
   
   With Damerau-Levenshtein, the misspelling has an edit distance of 1
   via transposition to "coordz1", and 2 to the other fields.  */

void foo (void)
{
  c.coorzd1 = c.coordy; /* { dg-error "has no member named 'coorzd1'; did you mean 'coordz1'" } */
}
