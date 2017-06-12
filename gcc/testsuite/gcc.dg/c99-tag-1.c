/* Test for handling of tags (6.7.2.3).  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  /* Forward declarations of structs and unions are OK; those of enums are
     not.  */
  {
    struct s0;
    struct s1 *x0;
    union u0;
    union u1 *x1;
    enum e0; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "forward" "enum forward 1" { target *-*-* } .-1 } */
    enum e1 *x2; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "forward" "enum forward 2" { target *-*-* } .-1 } */
    /* GCC used to fail to diagnose a use of an enum inside its definition.  */
    enum e2 { E2A = sizeof (enum e2 *) }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "forward" "enum forward 3" { target *-*-* } .-1 } */
  }
  /* A specific type shall have its content defined at most once.  But we
     may redeclare the tag in different scopes.  */
  {
    struct s0 { int i; }; /* { dg-message "note: originally defined here" } */
    {
      struct s0 { long l; };
    }
    {
      union s0 { long l; };
    }
    struct s0 { int i; }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "rede" "struct redef" { target *-*-* } .-1 } */
    union u0 { int i; }; /* { dg-message "note: originally defined here" } */
    {
      union u0 { long l; };
    }
    {
      struct u0 { long l; };
    }
    union u0 { int i; }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "rede" "union redef" { target *-*-* } .-1 } */
    enum e0 { E0A }; /* { dg-message "note: originally defined here" } */
    {
      enum e0 { E0B };
    }
    {
      struct e0 { long l; };
    }
    enum e0 { E0B }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "rede" "enum redef" { target *-*-* } .-1 } */
  }
  /* Structure, union and enumerated types have a single namespace of tags.  */
  {
    struct s0;
    struct s1;
    struct s2 { int i; };
    struct s2;
    struct s3 { int i; };
    struct s2 sv;
    union u0;
    union u2 { int i; };
    union u2;
    union u2 uv;
    enum e0 { E0A };
    enum e1 { E1A };
    /* None of the following are allowed; some were not detected by GCC.  */
    union s0; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    union s1 { int i; }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    union s2; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    union s3 { int i; }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    enum u0 { U0A }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    enum u2 { U2A }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    struct e0; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    struct e1 { int i; }; /* { dg-bogus "warning" "warning in place of error" } */
    /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
  }
  /* Explicit shadowing in inner scopes is OK, but references to the tag
     that don't explicitly shadow it must (whether in declarations or
     expressions) use the correct one of struct/union/enum.  */
  {
    struct s0;
    struct s1;
    struct s2 { int i; };
    struct s2;
    struct s3 { int i; };
    struct s2 sv;
    union u0;
    union u2 { int i; };
    union u2;
    union u2 uv;
    enum e0 { E0A };
    enum e1 { E1A };
    {
      union s0;
      union s1;
      union s2;
      union s3;
      struct u0;
      struct u2;
      struct e0;
      union e1;
    }
    {
      union s0 *x0; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      int x1[sizeof (union s1 *)]; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      struct t;
      union s2 *x2;
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      int x3[sizeof (union s3 *)]; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      struct u;
      enum u0 *y0; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong|forward" "wrong tag type" { target *-*-* } .-1 } */
      int y1[sizeof (enum u2 *)]; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong|forward" "wrong tag type" { target *-*-* } .-1 } */
      struct v;
      struct e0 *z0; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      int z1[sizeof (struct e1 *)]; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      struct w;
    }
    /* When explicitly shadowed to be a tag of a different type, references
       to the new type of tag must be accepted and those to the old type
       rejected.  */
    {
      union s0;
      union s0 *x0;
      union s1;
      struct s1 *x1; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
      union s2;
      union s2 *x2;
      union s3;
      struct s3 *x3; /* { dg-bogus "warning" "warning in place of error" } */
      /* { dg-error "wrong" "wrong tag type" { target *-*-* } .-1 } */
    }
  }
}
