struct sw {
  const void *x;
  int r;
};
struct sq {
  struct sw *q_w;
  int t;
  int z;
};

int
f (int ch, char *fp, char *ap)
{
  register int n;
  register char *cp;
  register struct sw *p;
  register int f;
  int prec;
  double _double;
  int expt;
  int ndig;
  char expstr[7];
  unsigned long long _uquad;
  struct sq q;
  struct sw w[8];
  static char zeroes[16];

  for (;;) {
    switch (ch) {
    case 'd':
      _double = (double) (ap += 8, *((double *) (ap - 8)));
      break;
    case 'o':
      goto nosign;
    case 'u':
      _uquad = (f & 0x020 ? (ap += 8, *((long long *) (ap - 8))) : f & 0x010 ? (ap += 4, *((long *) (ap - 4))) : f & 0x040 ? (long)(short)(ap += 4, *((int *) (ap - 4))) : (long)(ap += 4, *((int *) (ap - 4))));
      goto nosign;
    case 'x':
      _uquad = (f & 0x020 ? (ap += 8, *((long long *) (ap - 8))) : f & 0x010 ? (ap += 4, *((long *) (ap - 4))) : f & 0x040 ? (long)(unsigned short)(ap += 4, *((int *) (ap - 4))) : (long)(ap += 4, *((int *) (ap - 4))));
    nosign:
      if (_uquad != 0 || prec != 0);
      break;
    default:;
    }
    if ((f & 0x100) == 0) {
    } else {
      if (ch >= 'f') {
	if (_double == 0) {
	  if (expt < ndig || (f & 0x001) != 0) {
	    { if ((n = (ndig - 1)) > 0) { while (n > 16) {{ p->x = (zeroes); p->r = 16; q.z += 16; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }} n -= 16; }{ p->x = (zeroes); p->r = n; q.z += n; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}}}
	  }
	} else if (expt <= 0) {
	  { p->x = ("0"); p->r = 1; q.z += 1; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}
	  { p->x = 0; p->r = 1; q.z += 1; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}
	  { if ((n = (-expt)) > 0) { while (n > 16) {{ p->x = (zeroes); p->r = 16; q.z += 16; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }} n -= 16; }{ p->x = (zeroes); p->r = n; q.z += n; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }} }}
	  { p->x = cp; p->r = ndig; q.z += ndig; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}
	} else {
	  { p->x = cp; p->r = expt; q.z += expt; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}
	  cp += expt;
	  { p->x = ("."); p->r = 1; q.z += 1; p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}
	  { p->x = cp; p->r = (ndig-expt); q.z += (ndig-expt); p++; if (++q.t >= 8) { if (g(fp, &q)) goto error; p = w; }}
	}
      }
    }
  }

 error:;
}
