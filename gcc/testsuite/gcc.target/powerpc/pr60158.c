/* { dg-do compile } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } } */
/* { dg-options "-mdejagnu-cpu=8548 -mno-spe -mfloat-gprs=double -Os -fdata-sections -fpic -mrelocatable" } */

#define NULL 0
int func (int val);
void *func2 (void *ptr);

static const char *ifs;
static char map[256];

typedef struct {
/* None of these fields are used, but removing any
   of them makes the problem go away.  */
  char *data;
  int length;
  int maxlen;
  int quote;
} o_string;

#define NULL_O_STRING {NULL,0,0,0}

static int parse_stream (void *dest, void *ctx)
{
  int ch = func (0), m;

  while (ch != -1) {
    m = map[ch];
    if (ch != '\n')
    func2(dest);

    ctx = func2 (ctx);
    if (!func (0))
      return 0;
    if (m != ch) {
      func2 ("htns");
      break;
    }
  }
  return -1;
}

static void mapset (const char *set, int code)
{
  const char *s;
  for (s=set; *s; s++)  map[(int)*s] = code;
}

static void update_ifs_map(void)
{
  /* char *ifs and char map[256] are both globals.  */
  ifs = func2 ("abc");
  if (ifs == NULL) ifs="def";

  func2 (map);
  {
    char subst[2] = {4, 0};
    mapset (subst, 3);
  }
  mapset (";&|#", 1);
}

int parse_stream_outer (int flag)
{
  int blah;
  o_string temp=NULL_O_STRING;
  int rcode;

  do {
    update_ifs_map ();
    func2 (&blah); /* a memory clobber works as well.  */
    rcode = parse_stream (&temp, NULL);
    func2 ("aoeu");
    if (func (0) != 0) {
      func2 (NULL);
    }
  } while (rcode != -1);
  return 0;
}

/* { dg-final { if ![file exists pr60158.s] { fail "pr60158.c (compile)"; return; } } } */

/* { dg-final { set c_rel [llength [grep pr60158.s \\.data\\.rel\\.ro\\.local]] } } */
/* { dg-final { set c_fix [llength [grep pr60158.s \\.fixup]] } } */
/* { dg-final { if [string match $c_rel $c_fix] \{	} } */
/* { dg-final {     pass "pr60158.c (passed)"	} } */
/* { dg-final { \} else \{	} } */
/* { dg-final {     fail "pr60158.c (.fixup table entries not generated for .data.rel.ro.local section)"	} } */
/* { dg-final { \}	} } */
