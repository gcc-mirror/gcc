/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
    
struct rtx_def;
typedef struct rtx_def *rtx;



union rtunion_def
{
  int rtint;
};
typedef union rtunion_def rtunion;



struct rtx_def
{
  rtunion fld[1];

};

static int *uid_cuid;
static int max_uid_cuid;

rtx
bar (rtx r)
{
  rtx place = r;

  if (place->fld[0].rtint <= max_uid_cuid
      && (place->fld[0].rtint > max_uid_cuid ? insn_cuid (place) :
	  uid_cuid[place->fld[0].rtint]))
    return r;
  
  return 0;
}

/* There should be two IF conditionals.  One tests <= max_uid_cuid, the
   other tets the value in uid_cuid.  If either is false the jumps
   are threaded to the return 0.  Which in turn means the path
   which combines the result of those two tests into a new test
   must always be true and it is optimized appropriately.  */
/* { dg-final { scan-tree-dump-times "if " 2 "dom2"} } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
