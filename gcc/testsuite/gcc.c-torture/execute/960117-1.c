void exit (int);

static char id_space[2] [32 +1];
typedef short COUNT;

typedef char TEXT;

union T_VALS
{
  TEXT   *id __attribute__ ((aligned (2), packed)) ;
};
typedef union T_VALS VALS;

struct T_VAL
{
  COUNT    pos __attribute__ ((aligned (2), packed)) ;
  VALS    vals __attribute__ ((aligned (2), packed)) ;
};
typedef struct T_VAL VAL;

VAL curval = {0};

static short idc = 0;
static int cur_line;
static int char_pos;

typedef unsigned short WORD;

WORD
get_id (char c)
{
  curval.vals.id[0] = c;
}

WORD
get_tok ()
{
  char c = 'c';
  curval.vals.id = id_space[idc];
  curval.pos = (cur_line << 10) | char_pos;
  return get_id (c);
}

int
main (void)
{
  get_tok ();
  exit (0);
}
