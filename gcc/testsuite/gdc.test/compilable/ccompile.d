
/* REQUIRED_ARGS: -O
 */

// Adapted from DMC++ test file test3/ccompile.c


    struct HDS        {
      char state;
      uint done;
      uint retry;
      uint[15] tests;
   }

void funchds(char *p_adults)
{
   int cupx, chemx;
   HDS *p_cup;

   for (cupx = 1, p_cup = null; cupx <=  48 ; cupx ++, p_cup ++)
   {
         for (chemx = 0; chemx <  15 ; chemx++)
         {
            if (p_cup.done) {
               if (p_cup.tests [chemx]) {
                     *p_adults++ = 3;
               }
               if (p_cup.done && (p_cup.tests [chemx])) {
                     *p_adults++ = 4;
               }
            }
         }
   }
}
