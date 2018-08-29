/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2 -w" } */

struct rtx_def;
typedef struct rtx_def *rtx;
struct reload
{
  rtx in;
  rtx reg_rtx;
};
extern struct reload rld[(2 * 30 * (2 + 1))];
static rtx find_dummy_reload (rtx);
extern int frob ();
extern int arf ();
int
push_reload (rtx in, rtx out
)
{
  int i;
  if (out != 0 && in != out)
    {
      rld[i].reg_rtx = find_dummy_reload (out);
      if (rld[i].reg_rtx == out)
	 rld[i].in = out;
    }
}
rtx
find_dummy_reload (rtx real_out)
{
   unsigned int nwords = frob ();
   unsigned int regno = frob ();
   unsigned int i;
   for (i = 0; i < nwords; i++)
     if (arf ())
       break;
   if (i == nwords)
     return real_out;
  return 0;
}

/* In the case where the call to find_dummy_reload returns 0,
   the final test in push_reload will never be true and it will
   be eliminated.  */
/* { dg-final { scan-tree-dump-not "out_\[^\n\r]+ == 0" "dom2"} } */
