/* PR middle-end/22127 */
/* Testcase by <akr@m17n.org> */

/* { dg-do run { target sparc*-sun-solaris2.* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O" } */

typedef unsigned int size_t;
extern int printf(const char *, ...);
typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef unsigned int uint_t;
typedef char *caddr_t;
typedef int greg_t;
typedef greg_t gregset_t[19];
struct rwindow {
 greg_t rw_local[8];
 greg_t rw_in[8];
};
typedef struct gwindows {
 int wbcnt;
 greg_t *spbuf[31];
 struct rwindow wbuf[31];
} gwindows_t;
struct fpu {
 union {
  uint32_t fpu_regs[32];
  double fpu_dregs[16];
 } fpu_fr;
 struct fq *fpu_q;
 uint32_t fpu_fsr;
 uint8_t fpu_qcnt;
 uint8_t fpu_q_entrysize;
 uint8_t fpu_en;
};
typedef struct fpu fpregset_t;
typedef struct {
 unsigned int xrs_id;
 caddr_t xrs_ptr;
} xrs_t;
typedef struct {
 gregset_t gregs;
 gwindows_t *gwins;
 fpregset_t fpregs;
 xrs_t xrs;
 long filler[19];
} mcontext_t;
typedef struct {
 unsigned int __sigbits[4];
} sigset_t;
typedef struct sigaltstack {
 void *ss_sp;
 size_t ss_size;
 int ss_flags;
} stack_t;
typedef struct ucontext ucontext_t;
struct ucontext {
 uint_t uc_flags;
 ucontext_t *uc_link;
 sigset_t uc_sigmask;
 stack_t uc_stack;
 mcontext_t uc_mcontext;
 long uc_filler[23];
};
extern int getcontext(ucontext_t *);
extern int setcontext(const ucontext_t *);

int flag;
ucontext_t cont;
int pad[100];
typedef void (*fun_t)(int);
fun_t p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12;

int global;

extern void abort(void);

void h1(int v)
{
  global = v;
}

void h2(int v)
{
  if (v != 1)
    abort();
}

void f(void)
{
  flag = 1;
  setcontext(&cont);
}

int g(void)
{
  int ret;

  flag = 0;
  getcontext(&cont);
  ret = flag;
  if (ret == 0) {
    h1 (flag);
    p0 = p1 = p2 = p3 = p4 = p5 = p6 = p7 = p8 = h1;
    f();
    p0(ret); p1(ret); p2(ret); p3(ret); p4(ret); p5(ret); p6(ret); p7(ret); p8(ret);
  }
  else {
    h2 (flag);
  }
  return ret;
}

int main(void)
{
  g();
  return 0;
}
