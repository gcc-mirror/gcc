/* This testcase ICEd in combine.c:do_SUBST() self-test for sign-extended
CONST_INT because expr.c:expand_expr() was not sign-extending array index
into constant strings.  */

typedef unsigned char uch;
extern uch outbuf[];
extern unsigned outcnt;

extern void flush_outbuf (void);

int zip(void)
{
  outcnt = 0;

    {outbuf[outcnt++]=(uch)("\037\213"[0]); if (outcnt==16384) flush_outbuf();};
    {outbuf[outcnt++]=(uch)("\037\213"[1]); if (outcnt==16384) flush_outbuf();};

  return 0;
}

