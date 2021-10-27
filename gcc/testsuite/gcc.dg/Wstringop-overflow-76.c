/* Verify warnings and notes for MAX_EXPRs involving either pointers
   to distinct objects or one to a known object and the other to
   an unknown one.  Unlike for the same object, for unrelated objects
   the expected warnings and notes are the same as for MIN_EXPR: when
   the order of the objects in the address space cannot be determined
   the larger of them is assumed to be used.  (This is different for
   distinct struct members where the order is given.)
   The relational expressions are strictly invalid but that should be
   diagnosed by a separate warning.
   { dg-do compile }
   { dg-options "-O2 -Wno-array-bounds" } */

#define MAX(p, q) ((p) > (q) ? (p) : (q))

/* Verify that even for MAX_EXPR and like for MIN_EXPR, the note points
   to the larger of the two objects and mentions the offset into it
   (although the offset might be better included in the warning).  */
extern char a3[3];
extern char a5[5];  // { dg-message "at offset \[^a-zA-Z\n\r\]*5\[^a-zA-Z0-9\]* into destination object 'a5' of size 5" "note" }

void max_a3_a5 (int i)
{
  char *p = a3 + i;
  char *q = a5 + i;

  /* The relational expression below is invalid and should be diagnosed
     by its own warning independently of -Wstringop-overflow.  */
  char *d = MAX (p, q);

  d[2] = 0;         // { dg-warning "writing 4 bytes into a region of size 3" "pr102706" { target { vect_slp_v4qi_store } } }
  d[3] = 0;
  d[4] = 0;
  d[5] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "pr102706" { xfail { vect_slp_v4qi_store } } }
}


// Same as above but with the larger array as the first MAX_EXPR operand.
extern char b4[4];
extern char b6[6];  // { dg-message "at offset \[^a-zA-Z\n\r\]*6\[^a-zA-Z0-9\]* into destination object 'b6' of size 6" "note" }

void max_b6_b4 (int i)
{
  char *p = b6 + i;
  char *q = b4 + i;
  char *d = MAX (p, q);

  d[3] = 0;         // { dg-warning "writing 4 bytes into a region of size 3" "pr102706" { target { vect_slp_v4qi_store } } }
  d[4] = 0;
  d[5] = 0;
  d[6] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "pr102706" { xfail { vect_slp_v4qi_store } } }
}


/* Same as above but with the first MAX_EXPR operand pointing to an unknown
   object.  */
extern char c7[7];  // { dg-message "at offset 7 into destination object 'c7' of size 7" "note" }

void max_p_c7 (char *p, int i)
{
  char *q = c7 + i;
  char *d = MAX (p, q);

  d[6] = 0;
  d[7] = 0;         // { dg-warning "writing 1 byte into a region of size 0" }
}


/* Same as above but with the second MIN_EXPR operand pointing to an unknown
   object.  */
extern char d8[8];  // { dg-message "at offset 8 into destination object 'd8' of size 8" "note" }

void max_d8_p (char *q, int i)
{
  char *p = d8 + i;
  char *d = MAX (p, q);

  d[7] = 0;
  d[8] = 0;         // { dg-warning "writing 1 byte into a region of size 0" }
}


struct A3_5
{
  char a3[3];  // { dg-message "at offset 3 into destination object 'a3' of size 3" "pr??????" { xfail *-*-* } }
  // refer to pr102697 for xfail
  char a5[5];  // { dg-message "at offset 5 into destination object 'a5' of size 5" "note" { xfail { vect_slp_v4qi_store } } }
};

void max_A3_A5 (int i, struct A3_5 *pa3_5)
{
  char *p = pa3_5->a3 + i;
  char *q = pa3_5->a5 + i;

  char *d = MAX (p, q);

  d[2] = 0;
  d[3] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "pr??????" { xfail *-*-* } }
  d[4] = 0;
  d[5] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "pr102697" { xfail { vect_slp_v4qi_store } } }
}


struct B4_B6
{
  char b4[4];
  // refer to pr102697 for xfail
  char b6[6];       // { dg-message "at offset \[^a-zA-Z\n\r\]*6\[^a-zA-Z0-9\]* into destination object 'b6' of size 6" "note" { xfail { vect_slp_v4qi_store } } }
};

void max_B6_B4 (int i, struct B4_B6 *pb4_b6)
{
  char *p = pb4_b6->b6 + i;
  char *q = pb4_b6->b4 + i;
  char *d = MAX (p, q);

  d[3] = 0;
  d[4] = 0;
  d[5] = 0;
  d[6] = 0;         // { dg-warning "writing 1 byte into a region of size 0" "pr102697" { xfail { vect_slp_v4qi_store } } }
}


struct C7
{
  char c7[7];       // { dg-message "at offset 7 into destination object 'c7' of size 7" "note" }
};

void max_p_C7 (char *p, int i, struct C7 *pc7)
{
  char *q = pc7->c7 + i;
  char *d = MAX (p, q);

  d[6] = 0;
  d[7] = 0;         // { dg-warning "writing 1 byte into a region of size 0" }
}


struct D8
{
  char d8[8];       // { dg-message "at offset 8 into destination object 'd8' of size 8" "note" }
};

void max_D8_p (char *q, int i, struct D8 *pd8)
{
  char *p = pd8->d8 + i;
  char *d = MAX (p, q);

  d[7] = 0;
  d[8] = 0;         // { dg-warning "writing 1 byte into a region of size 0" }
}
