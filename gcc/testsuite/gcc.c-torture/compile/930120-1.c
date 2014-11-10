/* { dg-require-effective-target untyped_assembly } */
union {
  short I[2];
  long int L;
  char C[4];
} itolws;
char *errflg;
long int dot;
short dotinc;
long int expvf;

char *
f(fcount,ifp,itype,ptype)
     short fcount;
     char *ifp;
{
  unsigned w;
  long int savdot, wx;
  char *fp;
  char c, modifier, longpr;
  union {
    double dval;
    struct {
      int i1;
      int i2;
    } ival;
  } dw;
  union {
    float fval;
    int ival;
  } fw;
  int gotdot = 0;
  while (fcount > 0) {
    fp = ifp;
    c = *fp;
    longpr = ((c >= 'A') & (c <= 'Z') | (c == 'f') | (c == '4') | (c == 'p') | (c == 'i'));
    if ((itype == 0) || (*fp == 'a')) {
      wx = dot;
      w = dot;
    } else {
      gotdot = 1;
      wx = get((int)dot, itype);
      if (!longpr) {
	w = (itolws.L=(wx), itolws.I[((dot)&3)>>1]);
      }
    }
    if (c == 'F') {
      dw.ival.i1 = wx;
      if (itype == 0) {
	dw.ival.i2 = expvf;
      }
    }

    modifier = *fp++;
    switch(modifier) {
    case ' ' :
    case '\t' :
      break;
    case 't':
    case 'T':
      printf("%T",fcount);
      return(fp);
    case 'r':
    case 'R':
      printf("%M",fcount);
      return(fp);
    case 'k':
      printf("%k",w);
      break;
    case 'K':
      printf("%K",wx);
      break;
    case 'a':
      psymoff(dot,ptype,":%16t");
      dotinc = 0;
      break;
    case 'p':
      psymoff(0,ptype,"%16t");
      break;
    case 'u':
      printf("%-8u",w);
      break;
    case 'U':
      printf("%-16U",wx); break;
    case 'c':
    case 'C':
      if (modifier == 'C') {
	printesc((int)(itolws.L=(wx), itolws.C[(dot)&3]));
      } else {
	printc((char)(itolws.L=(wx), itolws.C[(dot)&3]));
      }
      dotinc = 1;
      break;
    case 'b':
      printf("%-8x", (itolws.L=(wx), itolws.C[(dot)&3]));
      dotinc = 1;
      break;
    case 'B':
      printf("%-8o", (itolws.L=(wx), itolws.C[(dot)&3]));
      dotinc = 1;
      break;
    case 's':
    case 'S':
      savdot = dot;
      dotinc = 1;
      while ((c = (itolws.L=(wx), itolws.C[(dot)&3])) && (errflg == 0)) {
	dot = inkdot(1);
	if (modifier == 'S') {
	  printesc(c);
	} else {
	  printc(c);
	}
	endline();
	if (!(dot & 3))
	  wx = get((int)dot, itype);
      }
      dotinc = dot - savdot + 1;
      dot = savdot;
      break;
    case 'i':
      if (gotdot) {
	wx = get((int)(dot & ~3), itype);
      }
      iDasm((int)(wx), (unsigned int)0, (unsigned int)(dot&~3));
      printc('\n');
      break;
    case 'f':
      fw.ival = wx;
      printf("%-16.9f", fw.fval);
      dotinc = 4;
      break;
    case 'F':
      printf("%-32.18F", dw.dval);
      dotinc = 8;
      break;
    }
  }
  return(fp);
}
