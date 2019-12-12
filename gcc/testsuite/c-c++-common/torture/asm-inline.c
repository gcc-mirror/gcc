/* { dg-do compile } */
/* -O0 does no inlining, and -O3 does it too aggressively for this test:  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O3" } { "" } }
/* The normal asm is not inlined:  */
/* { dg-final { scan-assembler-times "w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w.w" 2 } } */
/* But the asm inline is inlined:  */
/* { dg-final { scan-assembler-times "x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x" 8 } } */

static void f(void)
{
  asm ("w\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\n"
       "w\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw");
}

int f0(void) { f(); return 0; }
int f1(void) { f(); return 1; }
int f2(void) { f(); return 2; }
int f3(void) { f(); return 3; }

static void fg(void)
{
  asm goto("w\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\n"
	   "w\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw\nw" :::: q);
  q: ;
}

int fg0(void) { fg(); return 0; }
int fg1(void) { fg(); return 1; }
int fg2(void) { fg(); return 2; }
int fg3(void) { fg(); return 3; }

static void g(void)
{
  asm inline("x\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\n"
	     "x\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx");
}

int g0(void) { g(); return 0; }
int g1(void) { g(); return 1; }
int g2(void) { g(); return 2; }
int g3(void) { g(); return 3; }

static void gg(void)
{
  asm inline goto("x\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\n"
		  "x\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx" :::: q);
  q: ;
}

int gg0(void) { gg(); return 0; }
int gg1(void) { gg(); return 1; }
int gg2(void) { gg(); return 2; }
int gg3(void) { gg(); return 3; }
