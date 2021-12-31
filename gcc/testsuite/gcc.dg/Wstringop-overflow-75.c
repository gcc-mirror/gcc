/* Verify warnings and notes for MIN_EXPRs involving either pointers
   to distinct objects or one to a known object and the other to
   an unknown one.  The relational expressions are strictly invalid
   but that should be diagnosed by a separate warning.
   { dg-do compile }
   { dg-options "-O2 -Wno-array-bounds" } */

/* Verify the note points to the larger of the two objects and mentions
   the offset into it (although the offset might be better included in
   the warning).  */
extern char a3[3];
extern char a5[5];  // { dg-message "at offset \[^a-zA-Z\n\r\]*5\[^a-zA-Z0-9\]* into destination object 'a5' of size 5" "note" }

void min_a3_a5 (int i)
{
  char *p = a3 + i;
  char *q = a5 + i;

  /* The relational expression below is invalid and should be diagnosed
     by its own warning independently of -Wstringop-overflow.  */
  char *d = p < q ? p : q;

  d[4] = 0;         // { dg-warning "writing 2 bytes into a region of size 1" "" { target { vect_slp_v2qi_store_unalign } } }
  d[5] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}


// Same as above but with the larger array as the first MIN_EXPR operand.
extern char b4[4];
extern char b6[6];  // { dg-message "at offset \[^a-zA-Z\n\r\]*6\[^a-zA-Z0-9\]* into destination object 'b6' of size 6" "note" }

void min_b6_b4 (int i)
{
  char *p = b6 + i;
  char *q = b4 + i;
  char *d = p < q ? p : q;

  d[5] = 0;         // { dg-warning "writing 2 bytes into a region of size 1" "" { target { vect_slp_v2qi_store_unalign } } }
  d[6] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}


/* Same as above but with the first MIN_EXPR operand pointing to an unknown
   object.  */
extern char c7[7];  // { dg-message "at offset 7 into destination object 'c7' of size 7" "note" { xfail { vect_slp_v2qi_store_unalign } } }

void min_p_c7 (char *p, int i)
{
  char *q = c7 + i;
  char *d = p < q ? p : q;

  d[6] = 0;         // { dg-warning "writing 2 bytes into a region of size 1" "" { target { vect_slp_v2qi_store_unalign } } }
  d[7] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}


/* Same as above but with the second MIN_EXPR operand pointing to an unknown
   object.  */
extern char d8[8];  // { dg-message "at offset 8 into destination object 'd8' of size 8" "note" { xfail { vect_slp_v2qi_store_unalign } } }

void min_d8_p (char *q, int i)
{
  char *p = d8 + i;
  char *d = p < q ? p : q;

  d[7] = 0;         // { dg-warning "writing 2 bytes into a region of size 1" "" { target { vect_slp_v2qi_store_unalign } } }
  d[8] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}


struct A3_5
{
  char a3[3];
  char a5[5];  // { dg-message "at offset 5 into destination object 'a5' of size 5" "note" }
};

void min_A3_A5 (int i, struct A3_5 *pa3_5)
{
  char *p = pa3_5->a3 + i;
  char *q = pa3_5->a5 + i;

  char *d = p < q ? p : q;

  // d[4] = 0;
  d[5] = 0;         // { dg-warning "writing 1 byte into a region of size 0" }
}


struct B4_B6
{
  char b4[4];
  char b6[6];       // { dg-message "at offset 6 into destination object 'b6' of size 6" "note" { xfail { vect_slp_v2qi_store_unalign } } }
};

void min_B6_B4 (int i, struct B4_B6 *pb4_b6)
{
  char *p = pb4_b6->b6 + i;
  char *q = pb4_b6->b4 + i;
  char *d = p < q ? p : q;

  d[5] = 0;
  d[6] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}


struct C7
{
  char c7[7];       // { dg-message "at offset 7 into destination object 'c7' of size 7" "note" { xfail { vect_slp_v2qi_store_unalign } } }
};

void min_p_C7 (char *p, int i, struct C7 *pc7)
{
  char *q = pc7->c7 + i;
  char *d = p < q ? p : q;

  d[6] = 0;
  d[7] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}


struct D8
{
  char d8[8];       // { dg-message "at offset 8 into destination object 'd8' of size 8" "note" { xfail { vect_slp_v2qi_store_unalign } } }
};

void min_D8_p (char *q, int i, struct D8 *pd8)
{
  char *p = pd8->d8 + i;
  char *d = p < q ? p : q;

  d[7] = 0;
  d[8] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "" { xfail { vect_slp_v2qi_store_unalign } } }
}
