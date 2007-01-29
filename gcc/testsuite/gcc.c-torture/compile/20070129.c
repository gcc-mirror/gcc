/* This testcase would cause a hang in PTA solving due to a complex copy
   constraint and marking the wrong variable as changed.  */

typedef struct RExC_state_t
{
 char *end;
 char *parse;
} RExC_state_t;

struct regnode_string
{
 unsigned char str_len;
 char string[1];
};

static void *regatom (RExC_state_t * pRExC_state, int *flagp);

static void *
regpiece (RExC_state_t * pRExC_state, int *flagp)
{
 return regatom (0, 0);
}

static void *
regbranch (RExC_state_t * pRExC_state, int *flagp, int first)
{
 return regpiece (0, 0);
}

static void *
reg (RExC_state_t * pRExC_state, int paren, int *flagp)
{
 return regbranch (0, 0, 1);
}

void *
Perl_pregcomp (char *exp, char *xend, void *pm)
{
 return reg (0, 0, 0);
}

static void *
regatom (RExC_state_t * pRExC_state, int *flagp)
{
 register void *ret = 0;
 int flags;

tryagain:
 switch (*(pRExC_state->parse))
   {
   case '(':
     ret = reg (pRExC_state, 1, &flags);
     if (flags & 0x8)
       {
         goto tryagain;
       }
     break;
   default:
 {
       register unsigned long len;
       register unsigned ender;
       register char *p;
       char *oldp, *s;
       unsigned long numlen;
       unsigned long foldlen;
       unsigned char tmpbuf[6 + 1], *foldbuf;

     defchar:
       s = (((struct regnode_string *) ret)->string);
       for (len = 0, p = (pRExC_state->parse) - 1;
            len < 127 && p < (pRExC_state->end); len++)
         {
           if (((*p) == '*' || (*p) == '+' || (*p) == '?'
                || ((*p) == '{' && regcurly (p))))
             {
               unsigned long unilen;
               for (foldbuf = tmpbuf; foldlen; foldlen -= numlen)
                 {
                   reguni (pRExC_state, ender, s, &unilen);
                   s += unilen;
                 }
               break;
             }
           unsigned long unilen;

           reguni (pRExC_state, ender, s, &unilen);
           s += unilen;
         }

     };
     break;
   }
 return (ret);
}
