// { dg-do assemble  }
// Bug: g++ fails to catch incompatibilities in the parameter lists when
// assigning.

typedef struct S *type_p;
typedef struct S const *ctype_p;

typedef ctype_p (*PF) (int);

type_p callee (type_p arg) { return 0; }

void foobar ()
{
  static PF p = callee;		// { dg-error "" } 

  p = callee;			// { dg-error "" } 
}

PF pp = callee;			// { dg-error "" } 
