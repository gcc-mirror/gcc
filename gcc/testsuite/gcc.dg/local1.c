/* This is allowed, with the effect that the 'extern' declaration at block
   scope refers to the same object as the 'static' declaration at file scope.

      C90 6.1.2.2 [as corrected by TC1], C99 6.2.2:

	  For an identifier declared with the storage-class specifier
	  extern in a scope in which a prior declaration of that
	  identifier is visible, if the prior declaration specifies
	  internal or external linkage, the linkage of the identifier at
	  the later daclaration is the same as the linkage specified at
	  the prior declaration.  If no prior declaration is visible,
	  or if the prior declaration specifies no linkage, then the
	  identifer has external linkage.

   This is PR 14366.  */

static int i;

extern int i;

static void f() {
  extern int i;
}
