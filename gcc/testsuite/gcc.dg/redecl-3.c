/* Test for multiple declarations and composite types.  Includes bug
   13801.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-g" } */

typedef int IA[];
typedef int A10[10];

/* Test all combinations of: a variable declared at file scope (no
   type specifiers, or extern, or static), or just inside a function
   (with extern), redeclared in an inner scope (with extern), and
   redeclared in an inner scope when the previous declaration is
   hidden (with extern, and not if the original declaration was
   static).  Test three times: incomplete variable types; pointers to
   incomplete types; functions returning such pointers.  */

IA a0;
void
f0 (void)
{
  sizeof(a0); /* { dg-error "incomplete" } */
  {
    extern IA a0;
    sizeof(a0); /* { dg-error "incomplete" } */
    {
      int a0;
      {
        extern IA a0;
        sizeof(a0); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a0); /* { dg-error "incomplete" } */
  }
  sizeof(a0); /* { dg-error "incomplete" } */
}
extern A10 a0;

IA a1;
void
f1 (void)
{
  sizeof(a1); /* { dg-error "incomplete" } */
  {
    extern IA a1;
    sizeof(a1); /* { dg-error "incomplete" } */
    {
      int a1;
      {
        extern A10 a1;
        sizeof(a1);
      }
    }
    sizeof(a1); /* { dg-error "incomplete" } */
  }
  sizeof(a1); /* { dg-error "incomplete" } */
}
extern A10 a1;

IA a2;
void
f2 (void)
{
  sizeof(a2); /* { dg-error "incomplete" } */
  {
    extern A10 a2;
    sizeof(a2);
    {
      int a2;
      {
        extern IA a2;
        sizeof(a2); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a2);
  }
  sizeof(a2); /* { dg-error "incomplete" } */
}
extern A10 a2;

IA a3;
void
f3 (void)
{
  sizeof(a3); /* { dg-error "incomplete" } */
  {
    extern A10 a3;
    sizeof(a3);
    {
      int a3;
      {
        extern A10 a3;
        sizeof(a3);
      }
    }
    sizeof(a3);
  }
  sizeof(a3); /* { dg-error "incomplete" } */
}
extern A10 a3;

A10 a4;
void
f4 (void)
{
  sizeof(a4);
  {
    extern IA a4;
    sizeof(a4);
    {
      int a4;
      {
        extern IA a4;
        sizeof(a4); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a4);
  }
  sizeof(a4);
}
extern A10 a4;

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

A10 a6;
void
f6 (void)
{
  sizeof(a6);
  {
    extern A10 a6;
    sizeof(a6);
    {
      int a6;
      {
        extern IA a6;
        sizeof(a6); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a6);
  }
  sizeof(a6);
}
extern A10 a6;

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

extern IA a8;
void
f8 (void)
{
  sizeof(a8); /* { dg-error "incomplete" } */
  {
    extern IA a8;
    sizeof(a8); /* { dg-error "incomplete" } */
    {
      int a8;
      {
        extern IA a8;
        sizeof(a8); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a8); /* { dg-error "incomplete" } */
  }
  sizeof(a8); /* { dg-error "incomplete" } */
}
extern A10 a8;

extern IA a9;
void
f9 (void)
{
  sizeof(a9); /* { dg-error "incomplete" } */
  {
    extern IA a9;
    sizeof(a9); /* { dg-error "incomplete" } */
    {
      int a9;
      {
        extern A10 a9;
        sizeof(a9);
      }
    }
    sizeof(a9); /* { dg-error "incomplete" } */
  }
  sizeof(a9); /* { dg-error "incomplete" } */
}
extern A10 a9;

extern IA a10;
void
f10 (void)
{
  sizeof(a10); /* { dg-error "incomplete" } */
  {
    extern A10 a10;
    sizeof(a10);
    {
      int a10;
      {
        extern IA a10;
        sizeof(a10); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a10);
  }
  sizeof(a10); /* { dg-error "incomplete" } */
}
extern A10 a10;

extern IA a11;
void
f11 (void)
{
  sizeof(a11); /* { dg-error "incomplete" } */
  {
    extern A10 a11;
    sizeof(a11);
    {
      int a11;
      {
        extern A10 a11;
        sizeof(a11);
      }
    }
    sizeof(a11);
  }
  sizeof(a11); /* { dg-error "incomplete" } */
}
extern A10 a11;

extern A10 a12;
void
f12 (void)
{
  sizeof(a12);
  {
    extern IA a12;
    sizeof(a12);
    {
      int a12;
      {
        extern IA a12;
        sizeof(a12); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a12);
  }
  sizeof(a12);
}
extern A10 a12;

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

extern A10 a14;
void
f14 (void)
{
  sizeof(a14);
  {
    extern A10 a14;
    sizeof(a14);
    {
      int a14;
      {
        extern IA a14;
        sizeof(a14); /* { dg-error "incomplete" } */
      }
    }
    sizeof(a14);
  }
  sizeof(a14);
}
extern A10 a14;

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

static IA a16;
void
f16 (void)
{
  sizeof(a16); /* { dg-error "incomplete" } */
  {
    extern IA a16;
    sizeof(a16); /* { dg-error "incomplete" } */
  }
  sizeof(a16); /* { dg-error "incomplete" } */
}
extern A10 a16;

static IA a17;
void
f17 (void)
{
  sizeof(a17); /* { dg-error "incomplete" } */
  {
    extern A10 a17;
    sizeof(a17);
  }
  sizeof(a17); /* { dg-error "incomplete" } */
}
extern A10 a17;

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

IA *b0;
void
g0 (void)
{
  sizeof(*b0); /* { dg-error "incomplete" } */
  {
    extern IA *b0;
    sizeof(*b0); /* { dg-error "incomplete" } */
    {
      int b0;
      {
        extern IA *b0;
        sizeof(*b0); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b0); /* { dg-error "incomplete" } */
  }
  sizeof(*b0); /* { dg-error "incomplete" } */
}
extern A10 *b0;

IA *b1;
void
g1 (void)
{
  sizeof(*b1); /* { dg-error "incomplete" } */
  {
    extern IA *b1;
    sizeof(*b1); /* { dg-error "incomplete" } */
    {
      int b1;
      {
        extern A10 *b1;
        sizeof(*b1);
      }
    }
    sizeof(*b1); /* { dg-error "incomplete" } */
  }
  sizeof(*b1); /* { dg-error "incomplete" } */
}
extern A10 *b1;

IA *b2;
void
g2 (void)
{
  sizeof(*b2); /* { dg-error "incomplete" } */
  {
    extern A10 *b2;
    sizeof(*b2);
    {
      int b2;
      {
        extern IA *b2;
        sizeof(*b2); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b2);
  }
  sizeof(*b2); /* { dg-error "incomplete" } */
}
extern A10 *b2;

IA *b3;
void
g3 (void)
{
  sizeof(*b3); /* { dg-error "incomplete" } */
  {
    extern A10 *b3;
    sizeof(*b3);
    {
      int b3;
      {
        extern A10 *b3;
        sizeof(*b3);
      }
    }
    sizeof(*b3);
  }
  sizeof(*b3); /* { dg-error "incomplete" } */
}
extern A10 *b3;

A10 *b4;
void
g4 (void)
{
  sizeof(*b4);
  {
    extern IA *b4;
    sizeof(*b4);
    {
      int b4;
      {
        extern IA *b4;
        sizeof(*b4); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b4);
  }
  sizeof(*b4);
}
extern A10 *b4;

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

A10 *b6;
void
g6 (void)
{
  sizeof(*b6);
  {
    extern A10 *b6;
    sizeof(*b6);
    {
      int b6;
      {
        extern IA *b6;
        sizeof(*b6); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b6);
  }
  sizeof(*b6);
}
extern A10 *b6;

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

extern IA *b8;
void
g8 (void)
{
  sizeof(*b8); /* { dg-error "incomplete" } */
  {
    extern IA *b8;
    sizeof(*b8); /* { dg-error "incomplete" } */
    {
      int b8;
      {
        extern IA *b8;
        sizeof(*b8); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b8); /* { dg-error "incomplete" } */
  }
  sizeof(*b8); /* { dg-error "incomplete" } */
}
extern A10 *b8;

extern IA *b9;
void
g9 (void)
{
  sizeof(*b9); /* { dg-error "incomplete" } */
  {
    extern IA *b9;
    sizeof(*b9); /* { dg-error "incomplete" } */
    {
      int b9;
      {
        extern A10 *b9;
        sizeof(*b9);
      }
    }
    sizeof(*b9); /* { dg-error "incomplete" } */
  }
  sizeof(*b9); /* { dg-error "incomplete" } */
}
extern A10 *b9;

extern IA *b10;
void
g10 (void)
{
  sizeof(*b10); /* { dg-error "incomplete" } */
  {
    extern A10 *b10;
    sizeof(*b10);
    {
      int b10;
      {
        extern IA *b10;
        sizeof(*b10); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b10);
  }
  sizeof(*b10); /* { dg-error "incomplete" } */
}
extern A10 *b10;

extern IA *b11;
void
g11 (void)
{
  sizeof(*b11); /* { dg-error "incomplete" } */
  {
    extern A10 *b11;
    sizeof(*b11);
    {
      int b11;
      {
        extern A10 *b11;
        sizeof(*b11);
      }
    }
    sizeof(*b11);
  }
  sizeof(*b11); /* { dg-error "incomplete" } */
}
extern A10 *b11;

extern A10 *b12;
void
g12 (void)
{
  sizeof(*b12);
  {
    extern IA *b12;
    sizeof(*b12);
    {
      int b12;
      {
        extern IA *b12;
        sizeof(*b12); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b12);
  }
  sizeof(*b12);
}
extern A10 *b12;

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

extern A10 *b14;
void
g14 (void)
{
  sizeof(*b14);
  {
    extern A10 *b14;
    sizeof(*b14);
    {
      int b14;
      {
        extern IA *b14;
        sizeof(*b14); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*b14);
  }
  sizeof(*b14);
}
extern A10 *b14;

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

static IA *b16;
void
g16 (void)
{
  sizeof(*b16); /* { dg-error "incomplete" } */
  {
    extern IA *b16;
    sizeof(*b16); /* { dg-error "incomplete" } */
  }
  sizeof(*b16); /* { dg-error "incomplete" } */
}
extern A10 *b16;

static IA *b17;
void
g17 (void)
{
  sizeof(*b17); /* { dg-error "incomplete" } */
  {
    extern A10 *b17;
    sizeof(*b17);
  }
  sizeof(*b17); /* { dg-error "incomplete" } */
}
extern A10 *b17;

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

IA *c0 (void);
void
h0 (void)
{
  sizeof(*c0()); /* { dg-error "incomplete" } */
  {
    extern IA *c0 (void);
    sizeof(*c0()); /* { dg-error "incomplete" } */
    {
      int c0;
      {
        extern IA *c0 (void);
        sizeof(*c0()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c0()); /* { dg-error "incomplete" } */
  }
  sizeof(*c0()); /* { dg-error "incomplete" } */
}
A10 *c0 (void) { return 0; }

IA *c1 (void);
void
h1 (void)
{
  sizeof(*c1()); /* { dg-error "incomplete" } */
  {
    extern IA *c1 (void);
    sizeof(*c1()); /* { dg-error "incomplete" } */
    {
      int c1;
      {
        extern A10 *c1 (void);
        sizeof(*c1());
      }
    }
    sizeof(*c1()); /* { dg-error "incomplete" } */
  }
  sizeof(*c1()); /* { dg-error "incomplete" } */
}
A10 *c1 (void) { return 0; }

IA *c2 (void);
void
h2 (void)
{
  sizeof(*c2()); /* { dg-error "incomplete" } */
  {
    extern A10 *c2 (void);
    sizeof(*c2());
    {
      int c2;
      {
        extern IA *c2 (void);
        sizeof(*c2()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c2());
  }
  sizeof(*c2()); /* { dg-error "incomplete" } */
}
A10 *c2 (void) { return 0; }

IA *c3 (void);
void
h3 (void)
{
  sizeof(*c3()); /* { dg-error "incomplete" } */
  {
    extern A10 *c3 (void);
    sizeof(*c3());
    {
      int c3;
      {
        extern A10 *c3 (void);
        sizeof(*c3());
      }
    }
    sizeof(*c3());
  }
  sizeof(*c3()); /* { dg-error "incomplete" } */
}
A10 *c3 (void) { return 0; }

A10 *c4 (void);
void
h4 (void)
{
  sizeof(*c4());
  {
    extern IA *c4 (void);
    sizeof(*c4());
    {
      int c4;
      {
        extern IA *c4 (void);
        sizeof(*c4()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c4());
  }
  sizeof(*c4());
}
A10 *c4 (void) { return 0; }

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

A10 *c6 (void);
void
h6 (void)
{
  sizeof(*c6());
  {
    extern A10 *c6 (void);
    sizeof(*c6());
    {
      int c6;
      {
        extern IA *c6 (void);
        sizeof(*c6()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c6());
  }
  sizeof(*c6());
}
A10 *c6 (void) { return 0; }

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

extern IA *c8 (void);
void
h8 (void)
{
  sizeof(*c8()); /* { dg-error "incomplete" } */
  {
    extern IA *c8 (void);
    sizeof(*c8()); /* { dg-error "incomplete" } */
    {
      int c8;
      {
        extern IA *c8 (void);
        sizeof(*c8()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c8()); /* { dg-error "incomplete" } */
  }
  sizeof(*c8()); /* { dg-error "incomplete" } */
}
extern A10 *c8 (void) { return 0; }

extern IA *c9 (void);
void
h9 (void)
{
  sizeof(*c9()); /* { dg-error "incomplete" } */
  {
    extern IA *c9 (void);
    sizeof(*c9()); /* { dg-error "incomplete" } */
    {
      int c9;
      {
        extern A10 *c9 (void);
        sizeof(*c9());
      }
    }
    sizeof(*c9()); /* { dg-error "incomplete" } */
  }
  sizeof(*c9()); /* { dg-error "incomplete" } */
}
extern A10 *c9 (void) { return 0; }

extern IA *c10 (void);
void
h10 (void)
{
  sizeof(*c10()); /* { dg-error "incomplete" } */
  {
    extern A10 *c10 (void);
    sizeof(*c10());
    {
      int c10;
      {
        extern IA *c10 (void);
        sizeof(*c10()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c10());
  }
  sizeof(*c10()); /* { dg-error "incomplete" } */
}
extern A10 *c10 (void) { return 0; }

extern IA *c11 (void);
void
h11 (void)
{
  sizeof(*c11()); /* { dg-error "incomplete" } */
  {
    extern A10 *c11 (void);
    sizeof(*c11());
    {
      int c11;
      {
        extern A10 *c11 (void);
        sizeof(*c11());
      }
    }
    sizeof(*c11());
  }
  sizeof(*c11()); /* { dg-error "incomplete" } */
}
extern A10 *c11 (void) { return 0; }

extern A10 *c12 (void);
void
h12 (void)
{
  sizeof(*c12());
  {
    extern IA *c12 (void);
    sizeof(*c12());
    {
      int c12;
      {
        extern IA *c12 (void);
        sizeof(*c12()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c12());
  }
  sizeof(*c12());
}
extern A10 *c12 (void) { return 0; }

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

extern A10 *c14 (void);
void
h14 (void)
{
  sizeof(*c14());
  {
    extern A10 *c14 (void);
    sizeof(*c14());
    {
      int c14;
      {
        extern IA *c14 (void);
        sizeof(*c14()); /* { dg-error "incomplete" } */
      }
    }
    sizeof(*c14());
  }
  sizeof(*c14());
}
extern A10 *c14 (void) { return 0; }

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

static IA *c16 (void);
void
h16 (void)
{
  sizeof(*c16()); /* { dg-error "incomplete" } */
  {
    extern IA *c16 (void);
    sizeof(*c16()); /* { dg-error "incomplete" } */
  }
  sizeof(*c16()); /* { dg-error "incomplete" } */
}
static A10 *c16 (void) { return 0; }

static IA *c17 (void);
void
h17 (void)
{
  sizeof(*c17()); /* { dg-error "incomplete" } */
  {
    extern A10 *c17 (void);
    sizeof(*c17());
  }
  sizeof(*c17()); /* { dg-error "incomplete" } */
}
static A10 *c17 (void) { return 0; }

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
