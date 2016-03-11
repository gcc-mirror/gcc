/* { dg-options "-Wmisleading-indentation -Wall" } */
/* { dg-do compile } */

extern int foo (int);
extern int bar (int, int);
extern int flagA;
extern int flagB;
extern int flagC;
extern int flagD;

int
fn_1 (int flag)
{
  int x = 4, y = 5;
  if (flag) /* { dg-message "3: ...this 'if' clause, but it is not" } */
    x = 3;
    y = 2; /* { dg-warning "statement is indented as if it were guarded by..." } */
  return x * y;
}

int
fn_2 (int flag, int x, int y)
{
  if (flag) /* { dg-message "3: ...this 'if' clause, but it is not" } */
    x++; y++; /* { dg-warning "statement is indented as if it were guarded by..." } */

  return x * y;
}

int
fn_3 (int flag)
{
  int x = 4, y = 5;
  if (flag)
    x = 3;
  else /* { dg-message "3: ...this 'else' clause, but it is not" } */
    x = 2;
    y = 2; /* { dg-warning "statement is indented as if it were guarded by..." } */
  return x * y;
}

void
fn_4 (double *a, double *b, double *c)
{
  int i = 0;
  while (i < 10) /* { dg-message "3: ...this 'while' clause, but it is not" } */
    a[i] = b[i] * c[i];
    i++; /* { dg-warning "statement is indented as if it were guarded by..." } */
}

void
fn_5 (double *a, double *b, double *sum, double *prod)
{
  int i = 0;
  for (i = 0; i < 10; i++) /* { dg-output "3: ...this 'for' clause, but it is not" } */
    sum[i] = a[i] * b[i];
    prod[i] = a[i] * b[i]; /* { dg-warning "statement is indented as if it were guarded by..." } */
}

/* Based on CVE-2014-1266 aka "goto fail" */
int fn_6 (int a, int b, int c)
{
	int err;

	/* ... */
	if ((err = foo (a)) != 0)
		goto fail;
	if ((err = foo (b)) != 0) /* { dg-message "2: ...this 'if' clause, but it is not" } */
		goto fail;
		goto fail; /* { dg-warning "statement is indented as if it were guarded by..." } */
	if ((err = foo (c)) != 0)
		goto fail;
	/* ... */

fail:
	return err;
}

int fn_7 (int p, int q, int r, int s, int t)
{
  if (bar (p, q))
    {
      if (p) /* { dg-message "7: ...this 'if' clause, but it is not" } */
        q++; r++; /* { dg-warning "statement is indented as if it were guarded by..." } */
      t++;
    }
  return p + q + r + s + t;
}

int fn_8 (int a, int b, int c)
{
  /* This should *not* be flagged as misleading indentation.  */
  if (a) return b; else return c;
}

void fn_9 (int flag)
{
  if (flag) /* { dg-message "3: ...this 'if' clause, but it is not" } */
    foo (0);
    foo (1); /* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_10 (int flag)
{
  if (flag) /* { dg-message "3: ...this 'if' clause, but it is not" } */
    if (flag / 2)
      {
        foo (0);
        foo (1);
      }
    foo (2); /* { dg-warning "statement is indented as if it were guarded by..." } */
  foo (3);
}

void fn_11 (void)
{
  if (flagA)
    if (flagB)
      if (flagC) /* { dg-message "7: ...this 'if' clause, but it is not" } */
        foo (0);
        bar (1, 2); /* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_12 (void)
{
  if (flagA)
    if (flagB) /* { dg-message "5: ...this 'if' clause, but it is not" } */
      if (flagC)
        foo (0);
      bar (1, 2); /* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_13 (void)
{
  if (flagA) /* { dg-message "3: ...this 'if' clause, but it is not" } */
    if (flagB)
      if (flagC)
        foo (0);
    bar (1, 2); /* { dg-warning "statement is indented as if it were guarded by..." } */
}

#define FOR_EACH(VAR, START, STOP) \
  for ((VAR) = (START); (VAR) < (STOP); (VAR++)) /* { dg-message "3: ...this 'for' clause, but it is not" } */

void fn_14 (void)
{
  int i;
  FOR_EACH (i, 0, 10) /* { dg-message "3: in expansion of macro" } */
    foo (i);
    bar (i, i); /* { dg-warning "statement is indented as if it were guarded by..." } */
}
#undef FOR_EACH

#define FOR_EACH(VAR, START, STOP) for ((VAR) = (START); (VAR) < (STOP); (VAR++)) /* { dg-message "36: ...this 'for' clause, but it is not" } */
void fn_15 (void)
{
  int i;
  FOR_EACH (i, 0, 10) /* { dg-message "3: in expansion of macro" } */
    foo (i);
    bar (i, i); /* { dg-warning "statement is indented as if it were guarded by..." } */
}
#undef FOR_EACH

void fn_16_spaces (void)
{
  int i;
  for (i = 0; i < 10; i++)
    while (flagA)
      if (flagB) /* { dg-message "7: ...this 'if' clause, but it is not" } */
        foo (0);
        foo (1); /* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_16_tabs (void)
{
  int i;
  for (i = 0; i < 10; i++)
    while (flagA)
      if (flagB) /* { dg-message "7: ...this 'if' clause, but it is not" } */
	foo (0);
	foo (1);/* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_17_spaces (void)
{
  int i;
  for (i = 0; i < 10; i++) /* { dg-message "3: ...this 'for' clause, but it is not" } */
    while (flagA)
      if (flagB)
        foo (0);
    foo (1);/* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_17_tabs (void)
{
  int i;
  for (i = 0; i < 10; i++) /* { dg-message "3: ...this 'for' clause, but it is not" } */
    while (flagA)
      if (flagB)
	foo (0);
    foo (1);/* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_18_spaces (void)
{
  int i;
  for (i = 0; i < 10; i++)
    while (flagA) /* { dg-message "5: ...this 'while' clause, but it is not" } */
      if (flagB)
        foo (0);
      foo (1);/* { dg-warning "statement is indented as if it were guarded by..." } */
}

void fn_18_tabs (void)
{
  int i;
  for (i = 0; i < 10; i++)
    while (flagA) /* { dg-message "5: ...this 'while' clause, but it is not" } */
      if (flagB)
	foo (0);
      foo (1);/* { dg-warning "statement is indented as if it were guarded by..." } */
}

/* This shouldn't lead to a warning.  */
int fn_19 (void) { if (flagA) return 1; else return 0; }

/* A deeply-nested mixture of spaces and tabs, adapted from
   c-c++-common/pr60101.c.
   This should not lead to a warning.  */
void
fn_20 (unsigned int l)
{
  unsigned int i;

  for (i = 0; i < 10; i++)
    {
      unsigned int n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11;

      for (n0 = 0; n0 < l; n0++)
	for (n1 = 0; n1 < l; n1++)
	  for (n2 = 0; n2 < l; n2++)
	    for (n3 = 0; n3 < l; n3++)
	      for (n4 = 0; n4 < l; n4++)
		for (n5 = 0; n5 < l; n5++)
		  for (n6 = 0; n6 < l; n6++)
		    for (n7 = 0; n7 < l; n7++)
		      for (n8 = 0; n8 < l; n8++)
			for (n9 = 0; n9 < l; n9++)
			  for (n10 = 0; n10 < l; n10++)
			    for (n11 = 0; n11 < l; n11++)
			      {
				if (flagA)
				  foo (0);
				foo (1);
			      }
      foo (2);
    }
}

/* Another nested mix of tabs and spaces that shouldn't lead to a warning,
   with a preprocessor directive thrown in for good measure
   (adapted from libgomp/loop.c: gomp_loop_init).  */
void fn_21 (void)
{
  foo (0);
  if (flagA)
    {
      foo (1);

#if 1
      {
	foo (2);
	if (flagB)
	  {
	    if (flagC)
	      foo (3);
	    else
	      foo (4);
	  }
	else if (flagD)
	  foo (5);
	else
	  foo (6);
      }
#endif
    }
}

/* The conditionals within the following macros shouldn't be warned about.
   Adapted from libgomp/driver.c: gomp_load_plugin_for_device.  */
int fn_22 (void)
{
  int err = 0;

#define DLSYM()							\
  do									\
    {									\
      err = foo (0);							\
      if (err)								\
	goto out;							\
    }									\
  while (0)
#define DLSYM_OPT()							\
  do									\
    {									\
      err = foo (1);							\
      if (err)								\
        foo (2);							\
      else								\
        foo (3);							\
      foo (4);								\
    }									\
  while (0)
  DLSYM ();
  DLSYM_OPT ();
#undef DLSYM
#undef DLSYM_OPT

 out:
  return err;
}

/* This shouldn't be warned about.  */
void fn_23 (void) { foo (0); foo (1); if (flagA) foo (2); foo (3); foo (4); }

/* Code that simply doesn't bother indenting anywhere (e.g. autogenerated
   code) shouldn't be warned about.  */
void fn_24 (void)
{
  foo (0);
  if (flagA)
  foo (1);
  foo (2);
}

/* Adapted from libiberty/regex.c; an example of a conditional in a
   macro where the successor statement begins with a macro arg:

	    if (num < 0)
	      num = 0;
	    num = num * 10 + c - '0';
	    ^ this successor statement

   and hence "num" has a spelling location at the argument of the
   macro usage site ("lower_bound"), we want the definition of the
   parameter ("num") for the indentation comparison to be meaninful.

   This should not generate a misleading indentation warning.  */

# define GET_UNSIGNED_NUMBER(num) \
  {									\
    while (flagA)							\
      {									\
	if (flagB)						\
	  {								\
	    if (num < 0)						\
	      num = 0;							\
	    num = num * 10 + c - '0';					\
	  }								\
      }									\
  }
void fn_25 (int c, int lower_bound, int upper_bound)
{
  GET_UNSIGNED_NUMBER (lower_bound);
}
#undef GET_UNSIGNED_NUMBER

/* Example adapted from libdecnumber/decNumber.c:decExpOp that shouldn't
   trigger a warning.  */
void fn_26 (void)
{
  if (flagA) {
    if (flagB) foo (0); }
  foo (1);
}

/* Ensure that we don't get confused by mixed tabs and spaces; the line
   "foo (1);" has leading spaces before a tab, but this should not
   lead to a warning from -Wmisleading-indentation.  */
void fn_27 (void)
{
	      if (flagA)
		foo (0);
  	      foo (1);
}

/* Example adapted from gcc/cgraph.h:symtab_node::get_availability of
   a spurious trailing semicolon that shouldn't generate a warning.  */
void fn_28 (void)
{
  if (flagA)
    foo (0);
  else
    foo (1);;
}

/* However, other kinds of spurious semicolons can be a problem.  Sadly
   we don't yet report for the misleading-indented "foo (1);" in the
   following, due to the spurious semicolon.  */
void fn_29 (void)
{
  if (flagA)
    if (flagB)
      foo (0);;
    foo (1);
}

/* Adapted from usage site of #ifdef HAVE_cc0.  This should not lead
   to a warning from -Wmisleading-indentation.  */
void fn_30 (void)
{
  if (flagA)
    foo (0);
#if SOME_CONDITION_THAT_DOES_NOT_HOLD
  if (flagB)
#endif
    foo (1);
}

/* This shouldn't lead to a warning.  */
void fn_31 (void)
{
  if (flagA)
    foo (0);
  else if (flagB)
    foo (1);
  else if (flagC)
    foo (2);
  else
    foo (3);
}

/* Ensure that we can disable the warning.  */
int
fn_32 (int flag)
{
  int x = 4, y = 5;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmisleading-indentation"
  if (flag)
    x = 3;
    y = 2;
#pragma GCC diagnostic pop

  return x * y;
}

/* Verify that a variety of different indentation styles are supported
   without leading to warnings.  */
void
fn_33_k_and_r_style (void)
{
  int i;
  for (i = 0; i < 10; i++) {
    if (flagB) {
      foo(0);
      foo(1);
    } else {
      foo(2);
      foo(3);
    }
    foo(4);
  }
}

void
fn_33_stroustrup_style (void)
{
  int i;
  for (i = 0; i < 10; i++) {
    if (flagA) {
      foo(0);
      foo(1);
    }
    else {
      foo(2);
      foo(3);
    }
    foo(4);
  }
}

void
fn_33_allman_style (void)
{
  int i;
  for (i = 0; i < 10; i++)
  {
    if (flagA)
    {
      foo(0);
      foo(1);
    }
    else
    {
      foo(2);
      foo(3);
    }
    foo(4);
  }
}

void
fn_33_whitesmiths_style (void)
{
    int i;
    for (i = 0; i < 10; i++)
        {
        if (flagA)
            {
            foo(0);
            foo(1);
            }
        else
            {
            foo(2);
            foo(3);
            }
        foo(4);
        }
}

void
fn_33_horstmann_style (void)
{
    int i;
    for (i = 0; i < 10; i++)
    {   if (flagA)
        {   foo(0);
            foo(1);
        }
        else
        {   foo(2);
            foo(3);
        }
        foo(4);
    }
}

void
fn_33_ratliff_banner_style (void)
{
    int i;
    for (i = 0; i < 10; i++) {
       if (flagA) {
           foo(0);
           foo(1);
           }
       else {
            foo(2);
            foo(3);
            }
       foo(4);
       }
}

void
fn_33_lisp_style (void)
{
  int i;
  for (i = 0; i < 10; i++) {
    if (flagA) {
        foo(0);
        foo(1); }
    else {
        foo(2);
        foo(3); }
    foo(4); }
}

/* A function run through GNU "indent" with various options.
   None of these should lead to warnings.  */

/* "indent -gnu".  */
void
fn_34_indent_dash_gnu (void)
{
  int i;
  while (flagA)
    for (i = 0; i < 10; i++)
      {
	if (flagB)
	  {
	    foo (0);
	    foo (1);
	  }
	else
	  {
	    foo (2);
	    foo (3);
	  }
	foo (4);
      }
  foo (5);
}

/* "indent -kr".  */
void fn_34_indent_dash_kr(void)
{
    int i;
    while (flagA)
	for (i = 0; i < 10; i++) {
	    if (flagB) {
		foo(0);
		foo(1);
	    } else {
		foo(2);
		foo(3);
	    }
	    foo(4);
	}
    foo(5);
}

/* "indent -orig".  */
void
fn_34_indent_dash_orig(void)
{
    int             i;
    while (flagA)
	for (i = 0; i < 10; i++) {
	    if (flagB) {
		foo(0);
		foo(1);
	    } else {
		foo(2);
		foo(3);
	    }
	    foo(4);
	}
    foo(5);
}

/* Linux style:
   "indent \
      -nbad -bap -nbc -bbo -hnl -br -brs -c33 -cd33 -ncdb -ce -ci4  \
      -cli0 -d0 -di1 -nfc1 -i8 -ip0 -l80 -lp -npcs -nprs -npsl -sai \
      -saf -saw -ncs -nsc -sob -nfca -cp33 -ss -ts8 -il1".  */

void fn_34_indent_linux_style(void)
{
	int i;
	while (flagA)
		for (i = 0; i < 10; i++) {
			if (flagB) {
				foo(0);
				foo(1);
			} else {
				foo(2);
				foo(3);
			}
			foo(4);
		}
	foo(5);
}

/* PR 66220.  */
int fn_35 (int v)
{
    int res = 28;

    if (v == 2)
    {
        res = 27;
    } else
    {
        res = 18;
    }
    return res;
}

/* This variant of K&R-style formatting (in the presence of conditional
   compilation) shouldn't lead to a warning.

   Based on false positive seen with r223098 when compiling
   linux-4.0.3:arch/x86/crypto/aesni-intel_glue.c:aesni_init.  */
void
fn_36 (void)
{
#if 1 /* e.g. some configuration variable.  */
	if (flagA) {
		foo(0);
		foo(1);
		foo(2);
	} else
#endif
	{
		foo(3);
		foo(4);
		foo(5);
	}
	foo(6); /* We shouldn't warn here.  */
}

/* The following function contain code whose indentation is misleading, thus
   we warn about it.  */

void
fn_37 (void)
{
  int i;

#define EMPTY
#define FOR_EACH(VAR, START, STOP) for (VAR = START; VAR < STOP; VAR++)

  while (flagA); /* { dg-message "3: ...this 'while' clause" } */
    foo (0); /* { dg-warning "statement is indented as if" } */

  if (flagA)
    ;
  else if (flagB); /* { dg-message "8: ...this 'if' clause" } */
    foo (0); /* { dg-warning "statement is indented as if" } */
  while (flagA) /* { dg-message "3: ...this 'while' clause" } */
    /* blah */;
    foo (0); /* { dg-warning "statement is indented as if" } */

  if (flagA)
    ;
  else if (flagB) /* { dg-message "8: ...this 'if' clause" } */
    foo (1);
    foo (2); /* { dg-warning "statement is indented as if" } */

  if (flagA)
    foo (1);
  else if (flagB) /* { dg-message "8: ...this 'if' clause" } */
    foo (2);
    foo (3); /* { dg-warning "statement is indented as if" } */

  if (flagB) /* { dg-message "3: ...this 'if' clause" } */
    /* blah */;
    { /* { dg-warning "statement is indented as if" } */
      foo (0);
    }

  if (flagB) /* { dg-message "3: ...this 'if' clause" } */
    /* blah */;
   { /* { dg-warning "statement is indented as if" } */
     foo (0);
   }


  if (flagB)
    ;
  else; foo (0); /* { dg-warning "statement is indented as if" } */

  if (flagC); foo (2); /* { dg-warning "statement is indented as if" } */

  if (flagA)
    ; /* blah */ { /* { dg-warning "statement is indented as if" } */
      foo (1);
    }

  if (flagB) ; /* { dg-message "3: ...this 'if' clause" } */
    return; /* { dg-warning "statement is indented as if" } */

  if (flagB) EMPTY; /* { dg-message "3: ...this 'if' clause" } */
    foo (1); /* { dg-warning "statement is indented as if" } */

  for (i = 0; i < 10; i++); /* { dg-message "3: ...this 'for' clause" } */
    foo (2); /* { dg-warning "statement is indented as if" } */

  FOR_EACH (i, 0, 10);
    foo (2); /* { dg-warning "statement is indented as if" } */

  FOR_EACH (i, 0, 10);
    { /* { dg-warning "statement is indented as if" } */
      foo (3);
    }

  FOR_EACH (i, 0, 10);
  { /* { dg-warning "statement is indented as if" } */
    foo (3);
  }

  while (i++); { /* { dg-warning "statement is indented as if" } */
    foo (3);
  }

  if (i++); { /* { dg-warning "statement is indented as if" } */
    foo (3);
  }

  if (flagA) {
    foo (1);
  } else /* { dg-message "5: ...this 'else' clause" } */
    if (flagB)
       foo (2);
    foo (3); /* { dg-warning "statement is indented as if" } */

  if (flagA)
    foo (1);
  else if (flagB); /* { dg-message "8: ...this 'if' clause" } */
    foo (2); /* { dg-warning "statement is indented as if" } */

  for (i = 0; /* { dg-message "3: ...this 'for' clause" } */
       i < 10;
       i++);
    foo (i); /* { dg-warning "statement is indented as if" } */

  if (flagA)
  {
    foo (1);
  }
  else if (flagB); /* { dg-message "8: ...this 'if' clause" } */
  { /* { dg-warning "statement is indented as if" } */
    foo (2);
  }

#undef EMPTY
#undef FOR_EACH
}

/* The following function contains code whose indentation is not great but not
   misleading, thus we don't warn.  */

void
fn_38 (void)
{
  int i = 0;

  while (flagA)
    ;
    foo (0);

  if (flagB)
    ;
    {
      foo (0);
    }

  while (flagC);
  foo (2);

  if (flagA)
    while (flagC++);
  else
    foo (2);

  if (i)
    while (i++ < 10000);
  foo (5);

  if (i) while (i++ < 10000);
  foo (5);

  if (flagA) {
    foo (1);
  } else
  if (flagB)
    foo (2);
  foo (3);

  if (flagA)
    {
    foo (1);
    } else
  if (flagB)
    foo (2);
  foo (3);

  for (i = 0;
       i < 10;
       i++
  );
  foo (i);
}

/* The following function contains good indentation which we definitely should
   not warn about.  */

void
fn_39 (void)
{
  int i;

  if (flagA)
    ;
  if (flagB)
    ;

  if (flagA)
    if (flagB)
      foo (0);
    else
      foo (1);
  else
    foo (2);

  for (i = 0;
       i < 10;
       i++);
  foo (i);

  do foo (0); while (flagA);
}

/* We shouldn't complain about the following function.  */
#define emit
void pr69122 (void)
{
  if (flagA)
       foo (0);
  emit foo (1);
}
#undef emit

/* In the following, the 'if' within the 'for' statement is not indented,
   but arguably should be.
   The for loop:
     "for (cnt = 0; cnt < thousands_len; ++cnt)"
   does not guard this conditional:
     "cnt < thousands_len;".
   and the poor indentation is not misleading.  Verify that we do
   not erroneously emit a warning about this.
   Based on an example seen in glibc (PR c/68187).  */

void
fn_40_a (const char *end, const char *thousands, int thousands_len)
{
  int cnt;

  while (flagA)
    if (flagA
        && ({ for (cnt = 0; cnt < thousands_len; ++cnt)
              if (thousands[cnt] != end[cnt])
                break;
              cnt < thousands_len; })
        && flagB)
      break;
}

/* As above, but with the indentation within the "for" loop fixed.
   We should not emit a warning for this, either.  */

void
fn_40_b (const char *end, const char *thousands, int thousands_len)
{
  int cnt;

  while (flagA)
    if (flagA
        && ({ for (cnt = 0; cnt < thousands_len; ++cnt)
                if (thousands[cnt] != end[cnt])
                  break;
              cnt < thousands_len; })
        && flagB)
      break;
}

/* We should not warn for the following
   (based on libstdc++-v3/src/c++11/random.cc:random_device::_M_init).  */

void
fn_41_a (void)
{
  if (flagA)
    {
    }
  else if (flagB)
  fail:
    foo (0);

  foo (1);
  if (!flagC)
    goto fail;
}

/* Tweaked version of the above (with the label indented), which we should
   also not warn for.  */

void
fn_41_b (void)
{
  if (flagA)
    {
    }
  else if (flagB)
   fail:
    foo (0);

  foo (1);
  if (!flagC)
    goto fail;
}

/* In the following, the
     "if (i > 0)"
   is poorly indented, and ought to be on the same column as
      "engine_ref_debug(e, 0, -1)"
   However, it is not misleadingly indented, due to the presence
   of that macro.  Verify that we do not emit a warning about it
   not being guarded by the "else" clause above.

   Based on an example seen in OpenSSL 1.0.1, which was filed as
   PR c/68187 in comment #1, though it's arguably a separate bug to
   the one in comment #0.  */

int
fn_42_a (int locked)
{
#define engine_ref_debug(X, Y, Z)

    int i;

    if (locked)
        i = foo (0);
    else
        i = foo (1);
    engine_ref_debug(e, 0, -1)
        if (i > 0)
        return 1;
    return 0;
#undef engine_ref_debug
}

/* As above, but the empty macro is at the same indentation level.
   This *is* misleading; verify that we do emit a warning about it.  */

int
fn_42_b (int locked)
{
#define engine_ref_debug(X, Y, Z)

    int i;

    if (locked)
        i = foo (0);
    else /* { dg-message "...this .else. clause" } */
        i = foo (1);
        engine_ref_debug(e, 0, -1)
        if (i > 0) /* { dg-warning "statement is indented" } */
        return 1;
    return 0;
#undef engine_ref_debug
}

/* As above, but where the body is a semicolon "hidden" by a preceding
   comment, where the semicolon is not in the same column as the successor
   "if" statement, but the empty macro expansion is at the same indentation
   level as the guard.
   This is poor indentation, but not misleading; verify that we don't emit a
   warning about it.  */

int
fn_42_c (int locked, int i)
{
#define engine_ref_debug(X, Y, Z)

    if (locked)
        /* blah */;
    engine_ref_debug(e, 0, -1)
        if (i > 0)
        return 1;
    return 0;
#undef engine_ref_debug
}

/* We shouldn't complain about the following function.  */
#define ENABLE_FEATURE
int pr70085 (int x, int y)
{
  if (x > y)
    return x - y;

  #ifdef ENABLE_FEATURE
    if (x == y)
      return 0;
  #endif

  return -1;
}
#undef ENABLE_FEATURE
