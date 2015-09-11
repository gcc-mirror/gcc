/* { dg-do run } */

extern void abort (void);
extern void *memcpy(void *, const void *, __SIZE_TYPE__);

typedef struct {
    void *v1;
    void *v2;
    void *v3;
    union {
	void *f1;
	void *f2;
    } u;
} S;


S *getS();
void verify_p(void *p);
double *getP(void *p);

void memcpy_bug()
{
  S *s;
  double *p = getP(0);

  if (p) {
      int intSptr[sizeof(S*)/sizeof(int)];
      unsigned i = 0;
      for (i = 0; i < sizeof(intSptr)/sizeof(*intSptr); ++i) {
	  intSptr[i] = (int) p[i];
      }
      memcpy(&s, intSptr, sizeof(intSptr));
      (s)->u.f1 = p;
      verify_p((s)->u.f1);      
  } else {
      s = getS();
  }
  verify_p(s->u.f1);
}

double P[4];

double *getP(void *p) {
    union u {
	void *p;
	int i[2];
    } u;
    u.p = P;
    P[0] = u.i[0];
    P[1] = u.i[1];
    return P;
}

S *getS()
{
  return 0;
}

void verify_p(void *p)
{
  if (p != P)
    abort ();
}

int main(int argc, char *argv[])
{
    memcpy_bug();
    return 0;
}

