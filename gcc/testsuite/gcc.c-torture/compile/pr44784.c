typedef struct rtx_def *rtx;
enum rtx_code { SUBREG };
typedef union rtunion_def {
    long rtint;
    unsigned long rtuint;
    rtx rtx;
} rtunion;
struct rtx_def {
    enum rtx_code code: 8;
    rtunion fld[1];
};
typedef struct simple_bitmap_def {
    unsigned long long elms[1];
} *sbitmap;
struct df_link {
    struct df_link *next;
    rtx reg;
};
typedef enum { UNDEFINED,   CONSTANT,   VARYING } latticevalue;
typedef struct {
    latticevalue lattice_val;
} value;
static value *values;
static sbitmap ssa_edges;
void defs_to_varying (struct df_link *start)
{
  struct df_link *currdef;
  for (currdef = start;
       currdef;
       currdef = currdef->next)
    {
      rtx reg = currdef->reg;
      if (values[(reg->code == SUBREG
		  ? reg->fld[0].rtx
		  : reg)->fld[0].rtuint].lattice_val != VARYING)
	ssa_edges->elms [(reg->code == SUBREG
			  ? reg->fld[0].rtx
			  : reg)->fld[0].rtuint / 64]
	    |= ((unsigned long long) 1
		<< (reg->code == SUBREG
		    ? reg->fld[0].rtx
		    : reg)->fld[0].rtuint % 64);
      values[(reg->code == SUBREG
	      ? reg->fld[0].rtx
	      : reg)->fld[0].rtuint].lattice_val = VARYING;
    }
}
