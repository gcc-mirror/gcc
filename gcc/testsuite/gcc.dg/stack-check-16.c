/* { dg-do compile } */
/* { dg-options "-fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

int printf (const char *, ...);
void blah (char *space) { }
void mtrace (void);

int do_test (void)
{
    blah (__builtin_alloca (10));
    mtrace ();
    printf (
#define A(a) "%" #a "$s"
#define B(a) A(a)
#define C(a,b,c,d) B(a##b##c##d)
#define D(a,b,c) C(a,b,c,1) C(a,b,c,2) C(a,b,c,3) C(a,b,c,4) C(a,b,c,5) \
		 C(a,b,c,6) C(a,b,c,7) C(a,b,c,8) C(a,b,c,9)
#define E(a,b,c) C(a,b,c,0) D(a,b,c)
#define F(a,b) E(a,b,1) E(a,b,2) E(a,b,3) E(a,b,4) E(a,b,5) \
	       E(a,b,6) E(a,b,7) E(a,b,8) E(a,b,9)
#define G(a,b) E(a,b,0) F(a,b)
#define H(a) G(a,1) G(a,2) G(a,3) G(a,4) G(a,5) G(a,6) G(a,7) G(a,8) G(a,9)
#define I(a) G(a,0) H(a)
#define J I(1) I(2) I(3) I(4) I(5) I(6) I(7) I(8) I(9)
	    D(,,)
	    F(,)
	    H()
	    J
	    C(10,0,0,0) C(10,0,0,1),
#undef A
#define A(a) "a",
	    I(0) J
	    "\n");
  return 0;
}
