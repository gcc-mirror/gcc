/* Verify warnings and notes for MIN_EXPRs involving either pointers
   to distinct objects or one to a known object and the other to
   an unknown one.  The relational expressions are strictly invalid
   but that should be diagnosed by a separate warning.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds -Wno-stringop-overflow" } */

/* Verify the note points to the larger of the two objects and mentions
   the offset into it (alhough the offset would ideally be a part of
   the warning).  */
extern char a3[3];
extern char a5[5];  // { dg-message "at offset 5 into object 'a5' of size 5" "note" }

void min_a3_a5 (int i)
{
  char *p = a3 + i;
  char *q = a5 + i;

  /* The relational expression below is invalid and should be diagnosed
     by its own warning independently of -Warray-bounds.  */
  char *d = p < q ? p : q;

  d[4] = 0;

  /* Verify the type in the warning corresponds to the larger of the two
     objects.  */
  d[5] = 0;         // { dg-warning "subscript 5 is outside array bounds of 'char\\\[5]'" }
}


// Same as above but with the larger array as the first MIN_EXPR operand.
extern char b4[4];
extern char b6[6];  // { dg-message "at offset 6 into object 'b6' of size 6" "note" }

void min_b6_b4 (int i)
{
  char *p = b6 + i;
  char *q = b4 + i;
  char *d = p < q ? p : q;

  d[5] = 0;
  d[6] = 0;         // { dg-warning "subscript 6 is outside array bounds of 'char\\\[6]'" }
}


/* Same as above but with the first MIN_EXPR operand pointing to an unknown
   object.  */
extern char c7[7];  // { dg-message "at offset 7 into object 'c7' of size 7" "note" }

void min_p_c7 (char *p, int i)
{
  char *q = c7 + i;
  char *d = p < q ? p : q;

  d[6] = 0;
  d[7] = 0;         // { dg-warning "subscript 7 is outside array bounds of 'char\\\[7]'" }
}


/* Same as above but with the second MIN_EXPR operand pointing to an unknown
   object.  */
extern char d8[8];  // { dg-message "at offset 8 into object 'd8' of size 8" "note" }

void min_d8_p (char *q, int i)
{
  char *p = d8 + i;
  char *d = p < q ? p : q;

  d[7] = 0;
  d[8] = 0;         // { dg-warning "subscript 8 is outside array bounds of 'char\\\[8]'" }
}


/* The following are diagnosed by -Wstringop-overflow but, as a result
   of PR 101374, not by -Warray-bounds.  */

struct A3_5
{
  char a3[3];
  char a5[5];  // { dg-message "at offset 5 into object 'a5' of size 5" "note" { xfail *-*-* } }
};

void min_A3_A5 (int i, struct A3_5 *pa3_5)
{
  char *p = pa3_5->a3 + i;
  char *q = pa3_5->a5 + i;

  char *d = p < q ? p : q;

  // d[4] = 0;
  d[5] = 0;         // { dg-warning "subscript 5 is outside array bounds of 'char\\\[5]'" "pr??????" { xfail *-*-* } }
}


struct B4_B6
{
  char b4[4];
  char b6[6];       // { dg-message "at offset 6 into object 'b6' of size 6" "note" { xfail *-*-* } }
};

void min_B6_B4 (int i, struct B4_B6 *pb4_b6)
{
  char *p = pb4_b6->b6 + i;
  char *q = pb4_b6->b4 + i;
  char *d = p < q ? p : q;

  d[5] = 0;
  d[6] = 0;         // { dg-warning "subscript 6 is outside array bounds of 'char\\\[6]'" "pr??????" { xfail *-*-* } }
}


struct C7
{
  char c7[7];       // { dg-message "at offset 7 into object 'c7' of size 7" "note" { xfail *-*-* } }
};

void min_p_C7 (char *p, int i, struct C7 *pc7)
{
  char *q = pc7->c7 + i;
  char *d = p < q ? p : q;

  d[6] = 0;
  d[7] = 0;         // { dg-warning "subscript 7 is outside array bounds of 'char\\\[7]'" "pr??????" { xfail *-*-* } }
}


struct D8
{
  char d8[8];       // { dg-message "at offset 8 into object 'd8' of size 8" "note" { xfail *-*-* } }
};

void min_D8_p (char *q, int i, struct D8 *pd8)
{
  char *p = pd8->d8 + i;
  char *d = p < q ? p : q;

  d[7] = 0;
  d[8] = 0;         // { dg-warning "subscript 8 is outside array bounds of 'char\\\[8]'" "pr??????" { xfail *-*-* } }
}
