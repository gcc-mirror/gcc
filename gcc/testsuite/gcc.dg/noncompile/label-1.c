/* Test various diagnostics of ill-formed constructs involving labels.  */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

extern void dummy(void);

/* labels must be defined */
void a(void)
{
  goto l;   /* { dg-error "used but not defined" "no label" } */
}

/* warnings for labels defined but not used, or declared but not defined */
void b(void)
{
  __label__ l;
 l:  /* { dg-warning "defined but not used"  "no goto 1" } */
 m:  /* { dg-warning "defined but not used"  "no goto 2" } */
  dummy();
}

void c(void)
{
  __label__ l;  /* { dg-warning "declared but not defined" "only __label__" } */
  dummy();
}

/* can't have two labels with the same name in the same function */
void d(void)
{
 l: dummy();  /* { dg-error "previous definition" "prev def same scope" } */
 l: dummy();  /* { dg-error "duplicate label" "dup label same scope" } */
 goto l;
}

/* even at different scopes */
void e(void)
{
 l: dummy();	/* { dg-error "previous definition"  "prev def diff scope" } */
  {
  l: dummy();	/* { dg-error "duplicate label" "dup label diff scope" } */
  }
  goto l;
}

/* but, with __label__, you can */
void f(void)
{
 l: dummy();
  {
    __label__ l;
    l: dummy();	  /* { dg-warning "defined but not used" "unused shadow 1" } */
  };
  goto l;  /* this reaches the outer l */
}

/* a __label__ is not visible outside its scope */
void g(void)
{
  dummy();
  {
    __label__ l;
    l: dummy();
    goto l;
  }
  goto l;  /* { dg-error "used but not defined" "label ref out of scope" } */
}

/* __label__ can appear at top level of a function, too...
   ... but doesn't provide a definition of the label */
void h(void)
{
  __label__ l;
  dummy ();

  goto l;  /* { dg-error "used but not defined" "used, only __label__" } */
}

/* A nested function may not goto a label outside itself  */
void i(void)
{
  auto void nest(void);

 l: nest();
  
  void nest(void)
    {
      goto l;  /* { dg-error "used but not defined" "nest use outer label" } */
    }

  goto l; /* reaches the outer l */
}

/* which means that a nested function may have its own label with the
   same name as the outer function */
void j(void)
{
  auto void nest(void);

 l: nest();
  
  void nest(void)
    {
    l: dummy(); /* { dg-warning "defined but not used" "nest label same name" } */
    }

  goto l; /* reaches the outer l */
}

/* and, turnabout, an outer function may not goto a label in a nested
   function */
void k(void)
{
  void nest(void)
  {
  l: dummy();  /* { dg-warning "defined but not used" "outer use nest label" } */
  }

  goto l; /* { dg-error "used but not defined" "outer use nest label" } */
  nest();
}

/* not even with __label__ */
void l(void)
{
  void nest(void)
  {
    __label__ l;
  l: dummy(); /* { dg-warning "defined but not used" "outer use nest __label__" } */
  }

  goto l; /* { dg-error "used but not defined" "outer use nest __label__" } */
  nest();
}


/* but if the outer label is declared with __label__, then a nested
   function can goto that label (accomplishing a longjmp) */
void m(void)
{
  __label__ l;
  void nest(void) { goto l; }
  nest();
  dummy();
 l:;
}

/* and that means the nested function cannot have its own label with
   the same name as an outer label declared with __label__ */

void n(void)
{
  __label__ l; /* { dg-error "previous declaration" "outer label decl" } */
  void nest(void)
    {
    l: goto l;  /* { dg-error "duplicate label" "inner label defn" } */
    }

 l:
  nest();
}

/* unless the nested function uses __label__ too!  */
void o(void)
{
  __label__ l;
  void nest(void)
    {
      __label__ l;
    l: goto l;
    }

 l: goto l;
 nest();
}
