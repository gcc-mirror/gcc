/* Test for multiple declarations and composite types.  As in bug
   13801.  Test no problems in debug information generation.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int IA[];
typedef int A10[10];

/* Test all combinations of: a variable declared at file scope (no
   type specifiers, or extern, or static), or just inside a function
   (with extern), redeclared in an inner scope (with extern), and
   redeclared in an inner scope when the previous declaration is
   hidden (with extern, and not if the original declaration was
   static).  Test three times: incomplete variable types; pointers to
   incomplete types; functions returning such pointers.

   This test only includes the valid code cases, to test debug info
   generation.  (Incomplete static at file scope is not permitted by
   ISO C, but is accepted by GCC as an extension without
   -pedantic.)  */

A10 a5;
void
f5 (void)
{
  sizeof(a5);
  {
    extern IA a5;
    sizeof(a5);
    {
      int a5;
      {
        extern A10 a5;
        sizeof(a5);
      }
    }
    sizeof(a5);
  }
  sizeof(a5);
}
extern A10 a5;

A10 a7;
void
f7 (void)
{
  sizeof(a7);
  {
    extern A10 a7;
    sizeof(a7);
    {
      int a7;
      {
        extern A10 a7;
        sizeof(a7);
      }
    }
    sizeof(a7);
  }
  sizeof(a7);
}
extern A10 a7;

extern A10 a13;
void
f13 (void)
{
  sizeof(a13);
  {
    extern IA a13;
    sizeof(a13);
    {
      int a13;
      {
        extern A10 a13;
        sizeof(a13);
      }
    }
    sizeof(a13);
  }
  sizeof(a13);
}
extern A10 a13;

extern A10 a15;
void
f15 (void)
{
  sizeof(a15);
  {
    extern A10 a15;
    sizeof(a15);
    {
      int a15;
      {
        extern A10 a15;
        sizeof(a15);
      }
    }
    sizeof(a15);
  }
  sizeof(a15);
}
extern A10 a15;


static A10 a18;
void
f18 (void)
{
  sizeof(a18);
  {
    extern IA a18;
    sizeof(a18);
  }
  sizeof(a18);
}
extern A10 a18;

static A10 a19;
void
f19 (void)
{
  sizeof(a19);
  {
    extern A10 a19;
    sizeof(a19);
  }
  sizeof(a19);
}
extern A10 a19;

A10 *b5;
void
g5 (void)
{
  sizeof(*b5);
  {
    extern IA *b5;
    sizeof(*b5);
    {
      int b5;
      {
        extern A10 *b5;
        sizeof(*b5);
      }
    }
    sizeof(*b5);
  }
  sizeof(*b5);
}
extern A10 *b5;

A10 *b7;
void
g7 (void)
{
  sizeof(*b7);
  {
    extern A10 *b7;
    sizeof(*b7);
    {
      int b7;
      {
        extern A10 *b7;
        sizeof(*b7);
      }
    }
    sizeof(*b7);
  }
  sizeof(*b7);
}
extern A10 *b7;

extern A10 *b13;
void
g13 (void)
{
  sizeof(*b13);
  {
    extern IA *b13;
    sizeof(*b13);
    {
      int b13;
      {
        extern A10 *b13;
        sizeof(*b13);
      }
    }
    sizeof(*b13);
  }
  sizeof(*b13);
}
extern A10 *b13;

extern A10 *b15;
void
g15 (void)
{
  sizeof(*b15);
  {
    extern A10 *b15;
    sizeof(*b15);
    {
      int b15;
      {
        extern A10 *b15;
        sizeof(*b15);
      }
    }
    sizeof(*b15);
  }
  sizeof(*b15);
}
extern A10 *b15;

static A10 *b18;
void
g18 (void)
{
  sizeof(*b18);
  {
    extern IA *b18;
    sizeof(*b18);
  }
  sizeof(*b18);
}
extern A10 *b18;

static A10 *b19;
void
g19 (void)
{
  sizeof(*b19);
  {
    extern A10 *b19;
    sizeof(*b19);
  }
  sizeof(*b19);
}
extern A10 *b19;

A10 *c5 (void);
void
h5 (void)
{
  sizeof(*c5());
  {
    extern IA *c5 (void);
    sizeof(*c5());
    {
      int c5;
      {
        extern A10 *c5 (void);
        sizeof(*c5());
      }
    }
    sizeof(*c5());
  }
  sizeof(*c5());
}
A10 *c5 (void) { return 0; }

A10 *c7 (void);
void
h7 (void)
{
  sizeof(*c7());
  {
    extern A10 *c7 (void);
    sizeof(*c7());
    {
      int c7;
      {
        extern A10 *c7 (void);
        sizeof(*c7());
      }
    }
    sizeof(*c7());
  }
  sizeof(*c7());
}
A10 *c7 (void) { return 0; }

extern A10 *c13 (void);
void
h13 (void)
{
  sizeof(*c13());
  {
    extern IA *c13 (void);
    sizeof(*c13());
    {
      int c13;
      {
        extern A10 *c13 (void);
        sizeof(*c13());
      }
    }
    sizeof(*c13());
  }
  sizeof(*c13());
}
extern A10 *c13 (void) { return 0; }

extern A10 *c15 (void);
void
h15 (void)
{
  sizeof(*c15());
  {
    extern A10 *c15 (void);
    sizeof(*c15());
    {
      int c15;
      {
        extern A10 *c15 (void);
        sizeof(*c15());
      }
    }
    sizeof(*c15());
  }
  sizeof(*c15());
}
extern A10 *c15 (void) { return 0; }

static A10 *c18 (void);
void
h18 (void)
{
  sizeof(*c18());
  {
    extern IA *c18 (void);
    sizeof(*c18());
  }
  sizeof(*c18());
}
static A10 *c18 (void) { return 0; }

static A10 *c19 (void);
void
h19 (void)
{
  sizeof(*c19());
  {
    extern A10 *c19 (void);
    sizeof(*c19());
  }
  sizeof(*c19());
}
static A10 *c19 (void) { return 0; }
