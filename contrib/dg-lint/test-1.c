/* Various misspelled DejaGnu directives.  */

/* Underscore rather than dash.
   Example taken from gcc/testsuite/gcc.target/powerpc/vsx-builtin-msum.c
   fixed in r15-3878-gd5864b95ce94d9.  */
/* { dg_final { scan_assembler_times "vmsumudm" 2 } } */


/* Correct directive.  */
/* { dg-final { scan_assembler_times "vmsumudm" 2 } } */

/* Malformed uses of directives.

   Missing braces
   dg-output-file "m4.out"

   Missing leading brace
   dg-output-file "m4.out" }

   Missing trailing brace
   { dg-output-file "m4.out"

   Missing whitespace.
   {dg-output-file "m4.out"}

   Trailing comma.
   { dg-output-file, "m4.out" }
*/

/* Unrecognized directives.

   { dg-whatever }
   { dg-whatever  }
   { dg-whatever "" }
*/

/* Misspelled directive

   { dg-oupyt-file "m4.out" }

 */
