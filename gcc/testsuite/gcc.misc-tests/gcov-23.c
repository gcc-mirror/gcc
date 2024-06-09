/* { dg-options "-fcondition-coverage -ftest-coverage -O2 -c" } */

#include <stdint.h>
#include <limits.h>
#include <setjmp.h>
jmp_buf buf;

int id (int);
int idp (int *);
int err;
char c;

/* This becomes problematic only under optimization for the case when the
   compiler cannot inline the function but have to generate a call.  It is not
   really interesting to run, only build.  Notably, both the function calls and
   the return values are important to construct a problematic graph.

   This test is also a good example of where optimization makes condition
   coverage unpredictable, but not unusable.  If this is built without
   optimization the conditions work as you would expect from reading the
   source.  */
/* Adapted from cpio-2.14 gnu/utmens.c lutimens ().  */
int
mcdc001 (int *v)
{
    int adjusted;
    int adjustment_needed = 0;

    int *ts = v ? &adjusted : 0; /* conditions(0/4) true(0 1) false(0 1) */
				 /* conditions(end) */
    if (ts)
	adjustment_needed = idp (ts);
    if (adjustment_needed < 0)
	return -1;

    if (adjustment_needed)  /* conditions(0/2) true(0) false(0) */
			    /* conditions(end) */
    {
	if (adjustment_needed != 3) /* conditions(0/2) true(0) false(0) */
				    /* conditions(end) */
	    return -1;
	if (ts) /* conditions(0/2) true(0) false(0) */
		/* conditions(end) */
	    return 0;
    }

    if (adjustment_needed && idp (&adjusted)) /* conditions(0/4) true(0 1) false(0 1) */
					      /* conditions(end) */
	return -1;
    if (adjusted)   /* conditions(0/2) true(0) false(0) */
		    /* conditions(end) */
	return idp (ts);

    return -1;
}

/* This failed when the candidate set internal/contracted-past nodes were not
   properly marked as reachable in the candidate reduction phase.  */
/* Adapted from cpio-2.14 gnu/mktime.c mktime_internal ().  */
int
mcdc002 ()
{
    int a;
    if (idp (&a)) /* conditions(0/2) true(0) false(0) */
		  /* conditions(end) */
    {
	if (id (a)) /* conditions(0/2) true(0/2) true(0) false(0) */
		    /* conditions(end) */
	    goto exit;

	if (err) /* conditions(0/2) true(0/2) true(0) false(0) */
		 /* conditions(end) */
	    return -1;
    }

exit:
    return a;
}

/* Adapted from icu4c-73.1 common/ucase.cpp ucase_getCaseLocale ().  */
int
mcdc003 (const char *locale)
{
    /* extern, so its effect won't be optimized out.  */
    c = *locale++;
    if (c == 'z') /* conditions(0/2) true(0) false(0) */
		  /* conditions(end) */
    {
	return 1;
    }
    else if (c >= 'a') /* conditions(0/2) true(0) false(0) */
		      /* conditions(end) */
    {
	if (id (c)) /* conditions(0/2) true(0) false(0) */
		    /* conditions(end) */
	    c = *locale++;
    }
    else
    {
	if (c == 'T')
	{
	    if (id (c)) /* conditions(0/2) true(0) false(0) */
			/* conditions(end) */
		c = *locale++;
	    if (id (c)) /* conditions(0/2) true(0) false(0) */
			/* conditions(end) */
		c = *locale++;
	}
	/* This may or may not become a jump table.  */
	else if (c == 'L') /* conditions(suppress) */
			   /* conditions(end) */
	    c = *locale++;
	else if (c == 'E') /* conditions(suppress) */
			   /* conditions(end) */
	    c = *locale++;
	else if (c == 'N') /* conditions(suppress) */
			   /* conditions(end) */
	    c = *locale++;
	else if (c == 'H') /* conditions(suppress) */
			   /* conditions(end) */
	{
	    c = *locale++;
	    if (id (c)) /* conditions(0/2) true(0) false(0) */
			/* conditions(end) */
		c = *locale++;
	}
    }

    return 1;
}

/* The || will be changed to |, so it is impractical to predict the number of
   conditions.  If the walk is not properly implemented this will not finish
   compiling, so the actual coverage is not interesting.  */
/* Adapted from icu4c-73.1 common/uresdata.cpp res_findResource ().  */
int
mcdc004 (int r, char* path, char* key)
{
    char *idcc (char *, char);
    #define is_kind1(type) ((type) == 23 || (type) == 14 || (type == 115))
    #define is_kind2(type) ((type) == 16 || (type) == 77 || (type == 118))
    #define is_kind12(type) (is_kind1 ((type)) || is_kind2 ((type)))

    char *nextSepP = path;
    int t1 = r;
    int type = id (t1);

    if (!is_kind12 (type)) /* conditions(suppress) */
			   /* conditions(end) */
	return -1;

    while (*path && t1 != -1 && is_kind12 (type)) /* conditions(suppress) */
						  /* conditions(end) */
    {
	nextSepP = idcc(path, '/');
	if(nextSepP == path) /* conditions(0/2) true(0) false(0) */
			     /* conditions(end) */
	    return -1;

	if (*nextSepP == 'a') /* conditions(0/2) true(0) false(0) */
			      /* conditions(end) */
	    *key = *path;
	else if (*nextSepP == 'b')  /* conditions(0/2) true(0) false(0) */
				    /* conditions(end) */
	    *key = 0;
	type = t1;
    }

    return t1;
}

/* Adapted from jxl 0.8.2 lib/extras/dec/apng.cc processing_start ().
   This created a graph where depth-first traversal of the CFG would not
   process nodes in the wrong order (the extra control inserted from setjmp
   created a path of complexes from root to !b without going through !a).

   This only happened under optimization.  */
int
mcdc005 (int a, int b)
{
    a = id (a);
    b = id (b);

    /* The a || b gets transformed to a | b, then fused with setjmp because
       they both have the same return value.  */
    if (a || b) /* conditions(0/4) true(0 1) false(0 1) */
		/* conditions(end) */
	return 1;
    else
	a += 1;

    if (setjmp (buf))
	return 1;

    return a;
}

/* Adapted from cpio-2.14 gnu/quotearg.c quotearg_buffer_restyled.  The ifs in
   the cases (with fallthrough) re-use the same value which under optimization
   causes path reuse which must be sorted out.  */
int
mcdc006 (int quoting_style, int elide, int *buffer)
{
    int backslash = 0;
    switch (quoting_style)
    {
	case 1:
	    if (!elide)
		backslash = 1;
	case 2:
	    if (!elide)
		if (buffer)
		    *buffer = '"';
    }

    if (quoting_style == 2 && backslash)
	quoting_style = 1;
    return 1;
}

/* Adapted from pcre2-10.42 pcre2_compile.c pcre2_compile.  If SSA nodes are
   created at the wrong place in the block it will fail flow analysis (because
   the label is in the middle of block), caused by the final break in this
   case.  */
void
mcdc007 (int options, int *pso, int len)
{
    if (options == 5)
	return;

    while (options--)
    {
	int i;
	for (i = 0; i < len; i++)
	{
	    int *p = pso + i;
	    int skipatstart = *p + 2;
	    if (skipatstart) {
		switch(*p)
		{
		    case 1:
			*p |= *p + 1;
			break;
		    case 2:
			skipatstart += *p - skipatstart;
			break;
		}
		break;
	    }
	}
	if (i >= len) break;
    }
}

/* Adapted from alsa-lib 1.2.8 pcm/pcm.c snd_pcm_chmap_print.  */
int
mcdc008 (int *map, unsigned maxlen, int *buf)
{
    unsigned int len = 0;
    for (unsigned i = 0; i < *map; i++) {
	unsigned int p = map[i] & 0xF;
	if (i > 0) {
	    if (len >= maxlen)
		return -1;
	}
	if (map[i] & 0xFF)
	    len += idp (buf + len);
	else {
	    len += idp (buf);
	}
	if (map[i] & 0xFF00) {
	    len += idp (buf + len);
	    if (len >= maxlen)
		return -1;
	}
    }
    return len;
}

/* Adapted from cpio-2.14 gnu/mktime.c mktime_internal ().  The combination of
   goto, automatic variables, and the ternary causes the post dominator of the
   highest topological ordered node not to be the common post dominator of the
   expression as a whole.  */
int
mcdc009 (int *tp, int t, int isdst)
{
    int t0 = tp[0];
    int t1 = tp[1];
    int t2 = tp[2];

    if (t0 < 0 || (isdst < 0 ? t1 : (isdst != 0)))
	goto offset_found;

    if (t == 0)
	return -1;

    t1 = t2;

offset_found:
    return t;
}

/* Adapted from Berkeley db 4.8.30 rep/rep_elect.c __rep_cmp_vote.  This
   particular combination of fallthrough and folding creates a path into the
   the inner if () that does not go through the first basic condition.  */
void
mcdc010 (int cmp, int *rep, int sites, int priority, int flags)
{
    if (sites > 1 && (priority != 0 || (flags & 0xFF)))
    {
	if ( (priority != 0 && *rep == 0)
	|| (((priority == 0 && *rep == 0)
	||   (priority != 0 && *rep != 0)) && cmp > 0))
	{
	    *rep = cmp;
	}
    }
}

/* For not sufficiently protected back edges this would create an infinite
   loop.  */
void
mcdc011 (int a, int b)
{
    if (a && id (b))
	for (;;) {}
    id (a+1);
}

/* Adapted from alsa-1.2.8 tlv.c get_tlv_info ().  Under optimization, the
   conditions may be replaced with min ().  */
int
mcdc012 (int x, int y)
{
    int err;
    err = id (x);
    if (err < 0)
	return err;
    err = id (y);
    if (err < 0)
	return err;
    return 0;
}

/* Adapted from alsa-1.2.8 control.c snd_ctl_elem_id_compare_numid ().  This
   test is probably not so accurate on targets where int == int64.  Under
   optimization, the conditions may be replaced with min/max.   */
int
mcdc013 (const int64_t *id1, const int64_t *id2)
{
    int64_t d;
    d = *id1 - *id2;
    if (d & 0xFF)
    {
	if (d > INT_MAX)
	    d = INT_MAX;
	else if (d < INT_MIN)
	    d = INT_MIN;
    }
    return d;
}
