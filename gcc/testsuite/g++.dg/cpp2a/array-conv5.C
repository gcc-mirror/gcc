// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do run { target c++2a } }

// Ranking of list-initialization sequences
int b(int   (&&)[] ) { return 1; }   // #1
int b(long  (&&)[] ) { return 2; }   // #2
int b(int   (&&)[1]) { return 3; }   // #3
int b(long  (&&)[1]) { return 4; }   // #4
int b(int   (&&)[2]) { return 5; }   // #5

/* Here,
   -- #1, #3 and #5 should rank better than both #2 and #4, as no promotion
      is necessitated.
   -- #1 should rank worse than #3, being far less specialized.
   -- #1 should rank better than #5, as the latter requires a larger array
      temporary.  (#3 also ranks better than #5 for the same reason--cf. core
      issue 1307).  */

int
main ()
{
  if (b({1}) != 3)
    __builtin_abort ();
}
