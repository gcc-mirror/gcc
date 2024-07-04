// PR c/37772
// { dg-do compile }
// { dg-options "" }

void
foo ()
{
  int i;
  asm ();		  // { dg-error "expected string-literal" }
  asm (1);		  // { dg-error "expected string-literal" }
  asm (int);		  // { dg-error "expected string-literal" }
  asm (: "=r" (i));	  // { dg-error "expected string-literal" }
  asm (1 : "=r" (i));	  // { dg-error "expected string-literal" }
  asm (int : "=r" (i));	  // { dg-error "expected string-literal" }
  asm (: : "r" (i));	  // { dg-error "expected string-literal" }
  asm (1 : : "r" (i));	  // { dg-error "expected string-literal" }
  asm (int : : "r" (i));  // { dg-error "expected string-literal" }
  asm (: : : "memory");	  // { dg-error "expected string-literal" }
  asm (1 : : : "memory"); // { dg-error "expected string-literal" }
}
