/* Variables and structures for overloading rules.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* The following structure is used when comparing various alternatives
   for overloading.  The unsigned quantity `strikes.i' is used
   for fast comparison of two possibilities.  This number is an
   aggregate of four constituents:

     EVIL: if this is non-zero, then the candidate should not be considered
     ELLIPSIS: if this is non-zero, then some actual argument has been matched
               against an ellipsis
     USER: if this is non-zero, then a user-defined type conversion is needed
     B_OR_D: if this is non-zero, then use a base pointer instead of the
             type of the pointer we started with.
     EASY: if this is non-zero, then we have a builtin conversion
           (such as int to long, int to float, etc) to do.

   If two candidates require user-defined type conversions, and the
   type conversions are not identical, then an ambiguity error
   is reported.

   If two candidates agree on user-defined type conversions,
   and one uses pointers of strictly higher type (derived where
   another uses base), then that alternative is silently chosen.

   If two candidates have a non-monotonic derived/base pointer
   relationship, and/or a non-monotonic easy conversion relationship,
   then a warning is emitted to show which paths are possible, and
   which one is being chosen.

   For example:

   int i;
   double x;

   overload f;
   int f (int, int);
   double f (double, double);

   f (i, x);	// draws a warning

   struct B
   {
     f (int);
   } *bb;
   struct D : B
   {
     f (double);
   } *dd;

   dd->f (x);	// exact match
   dd->f (i);	// draws warning

   Note that this technique really only works for 255 arguments.  Perhaps
   this is not enough.  */

/* These macros and harshness_code are used by the NEW METHOD.  */
#define EVIL_CODE (1<<7)
#define CONST_CODE (1<<6)
#define ELLIPSIS_CODE (1<<5)
#define USER_CODE (1<<4)
#define STD_CODE (1<<3)
#define PROMO_CODE (1<<2)
#define QUAL_CODE (1<<1)
#define TRIVIAL_CODE (1<<0)

struct harshness_code
{
  /* What kind of conversion is involved.  */
  unsigned short code;

  /* The inheritance distance.  */
  short distance;

  /* For a PROMO_CODE, Any special penalties involved in integral conversions.
     This exists because $4.1 of the ARM states that something like
     `short unsigned int' should promote to `int', not `unsigned int'.
     If, for example, it tries to match two fns, f(int) and f(unsigned),
     f(int) should be a better match than f(unsigned) by this rule.  Without
     this extra metric, they both only appear as "integral promotions", which
     will lead to an ambiguity.
     For a TRIVIAL_CODE, This is also used by build_overload_call_real and
     convert_harshness to keep track of other information we need.  */
  unsigned short int_penalty;
};

struct candidate
{
  /* OLD METHOD */
  unsigned char evil;	      /* !0 if this will never convert.  */
  unsigned char ellipsis;     /* !0 if a match against an ellipsis occurred */
  unsigned char user;	      /* !0 if at least one user-defined type conv.  */
  unsigned short b_or_d;      /* count number of derived->base or
				 base->derived conv.  */
  unsigned short easy;	      /* count number of builtin type conv.  */

  /* NEW METHOD */
  struct harshness_code h;	/* Used for single-argument conversions.  */

  int h_len;			/* The length of the harshness vector.  */

  /* Both methods.  */
  tree function;		/* A FUNCTION_DECL */
  tree basetypes;		/* The path to function. */
  tree arg;			/* first parm to function.  */

  /* This union is only here while we maintain both the old and new
     argument matching schemes.  When it goes away, all v.ansi_harshness
     references will be just `harshness'.  */
  union
    {
      /* Indexed by argument number, encodes evil, user, d_to_b, and easy
	 strikes for that argument.  At end of array, we store the index+1
	 of where we started using default parameters, or 0 if there are
	 none.  */
      struct harshness_code *ansi_harshness; /* NEW METHOD */
      unsigned short *old_harshness;  /* OLD METHOD */
    } v;

  union
    {
      tree field;		/* If no evil strikes, the FUNCTION_DECL of
				   the function (if a member function).  */
      int bad_arg;		/* the index of the first bad argument:
				   0 if no bad arguments
				   > 0 is first bad argument
				   -1 if extra actual arguments
				   -2 if too few actual arguments.
				   -3 if const/non const method mismatch.
				   -4 if type unification failed.
				   -5 if contravariance violation.  */
    } u;
};
int rank_for_overload ();

/* Variables shared between class.c and call.c.  */

extern int n_vtables;
extern int n_vtable_entries;
extern int n_vtable_searches;
extern int n_vtable_elems;
extern int n_convert_harshness;
extern int n_compute_conversion_costs;
extern int n_build_method_call;
extern int n_inner_fields_searched;
