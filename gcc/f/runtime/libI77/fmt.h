struct syl
{	int op;
	int p1;
	union { int i[2]; char *s;} p2;
	};
#define RET1 1
#define REVERT 2
#define GOTO 3
#define X 4
#define SLASH 5
#define STACK 6
#define I 7
#define ED 8
#define NED 9
#define IM 10
#define APOS 11
#define H 12
#define TL 13
#define TR 14
#define T 15
#define COLON 16
#define S 17
#define SP 18
#define SS 19
#define P 20
#define BN 21
#define BZ 22
#define F 23
#define E 24
#define EE 25
#define D 26
#define G 27
#define GE 28
#define L 29
#define A 30
#define AW 31
#define O 32
#define NONL 33
#define OM 34
#define Z 35
#define ZM 36
extern int f__pc,f__parenlvl,f__revloc;
typedef union
{	real pf;
	doublereal pd;
} ufloat;
typedef union
{	short is;
#ifndef KR_headers
	signed
#endif
		char ic;
	integer il;
#ifdef Allow_TYQUAD
	longint ili;
#endif
} Uint;
#ifdef KR_headers
extern int (*f__doed)(),(*f__doned)();
extern int (*f__dorevert)();
extern int rd_ed(),rd_ned();
extern int w_ed(),w_ned();
#else
#ifdef __cplusplus
extern "C" {
#endif
extern int (*f__doed)(struct syl*, char*, ftnlen),(*f__doned)(struct syl*);
extern int (*f__dorevert)(void);
extern void fmt_bg(void);
extern int pars_f(char*);
extern int rd_ed(struct syl*, char*, ftnlen),rd_ned(struct syl*);
extern int w_ed(struct syl*, char*, ftnlen),w_ned(struct syl*);
extern int wrt_E(ufloat*, int, int, int, ftnlen);
extern int wrt_F(ufloat*, int, int, ftnlen);
extern int wrt_L(Uint*, int, ftnlen);
#ifdef __cplusplus
	}
#endif
#endif
extern flag f__cblank,f__cplus,f__workdone, f__nonl;
extern char *f__fmtbuf;
extern int f__fmtlen;
extern int f__scale;
#define GET(x) if((x=(*f__getn)())<0) return(x)
#define VAL(x) (x!='\n'?x:' ')
#define PUT(x) (*f__putn)(x)
extern int f__cursor;

#undef TYQUAD
#ifndef Allow_TYQUAD
#undef longint
#define longint long
#else
#define TYQUAD 14
#endif

#ifdef KR_headers
extern char *f__icvt();
#else
extern char *f__icvt(longint, int*, int*, int);
#endif
