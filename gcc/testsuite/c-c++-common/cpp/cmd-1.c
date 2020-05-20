/* { dg-do preprocess }
   { dg-options "-Dfrob=drob -Ddrob=BOB f.c -E -fdirectives-only -include cmd-1.h" }
*/

#define BOB bob()
#define bob() dob

frob

/* Ensure fidelity of the preprocessed output.  */
/* { dg-final { scan-file cmd-1.o {# 0 "<command-line>"\n
#define frob drob\n# 0 "<command-line>"\n#define drob BOB\n# 0 "<command-line>"\n# 1 "./h.h" 1\nb\n# 0 "<command-line>" 2\n# 1 "f.c"\n\n#define BOB bob()\n#define bob() dob\n} } }
 */
