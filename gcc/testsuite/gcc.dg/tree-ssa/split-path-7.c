/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-paths -fdump-tree-split-paths-details -w" } */


struct _reent
{
};
typedef unsigned char ichar_t;
struct dent
{
  char *word;
};
struct flagent
{
  ichar_t *strip;
  ichar_t *affix;
  short stripl;
  short affl;
};
union ptr_union
{
  struct flagptr *fp;
  struct flagent *ent;
};
struct flagptr
{
  union ptr_union pu;
  int numents;
};
struct hashheader
{
};
extern struct dent *hashtbl;
extern char *hashstrings;
extern int hashsize;
extern int nodictflag;
extern int numsflags;
extern int numpflags;
extern struct flagent *pflaglist;
extern struct flagent *sflaglist;
int
linit ()
{
  register int i;
  register struct dent *dp;
  struct flagent *entry;
  struct flagptr *ind;
  int viazero;
  register ichar_t *cp;
  if (!nodictflag)
    {
      for (i = hashsize, dp = hashtbl; --i >= 0; dp++)
	{
	  if (dp->word == (char *) -1)
	    dp->word = ((void *) 0);
	  else
	    dp->word = &hashstrings[(int) (dp->word)];
	}
    }
  for (i = numsflags + numpflags, entry = sflaglist; --i >= 0; entry++)
    {
      if (entry->stripl)
	entry->strip = (ichar_t *) & hashstrings[(int) entry->strip];
      else
	entry->affix = ((void *) 0);
    }
  for (i = numsflags, entry = sflaglist; i > 0; i--, entry++)
    {
      if (entry->affl == 0)
	{
	  if (ind->pu.fp == ((void *) 0))
	    {
	    }
	}
    }
  for (i = numpflags, entry = pflaglist; i > 0; i--, entry++)
    {
      if (entry->affl == 0)
	{
	  while (ind->numents == 0 && ind->pu.fp != ((void *) 0))
	    {
	      if (*cp == 0)
		{
		}
	    }
	}
      if (!viazero && ind->numents >= 4
	  && strcmp ((char *) (entry->affix),
		     (char *) (ind->pu.ent->affix)) != 0)
	{
	}
    }
}
/* { dg-final { scan-tree-dump-times "Duplicating join block" 0 "split-paths" } } */
