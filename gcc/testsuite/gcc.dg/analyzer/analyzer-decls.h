#ifndef ANALYZER_DECLS_H
#define ANALYZER_DECLS_H

#ifndef NULL
#ifdef __cplusplus
#if __cplusplus >= 201103L
#define NULL nullptr
#else
#define NULL 0
#endif
#else
#define NULL ((void *)0)
#endif
#endif

/* Function decls with special meaning to the analyzer.
   None of these are actually implemented.  */

/* Trigger a breakpoint in the analyzer when reached.  */
extern void
__analyzer_break (void)
  __attribute__ ((nothrow));

/* Emit a warning describing the 2nd argument (which can be of any
   type), at the given verbosity level.  This is for use when
   debugging, and may be of use in DejaGnu tests.  */
extern void
__analyzer_describe (int verbosity, ...)
  __attribute__ ((nothrow));

/* Dump copious information about the analyzer’s state when reached.  */
extern void
__analyzer_dump (void)
  __attribute__ ((nothrow));

/* Emit a warning describing the size of the base region of (*ptr).  */
extern void
__analyzer_dump_capacity (const void *ptr)
  __attribute__ ((nothrow));

/* When reached, dump GraphViz .dot source to stderr for a diagram
   describing the analyzer’s state.  */
extern void
__analyzer_dump_dot (void)
  __attribute__ ((nothrow));

/* Dump information about what decls have escaped at this point on the path.  */
extern void
__analyzer_dump_escaped (void)
  __attribute__ ((nothrow));

/* Dump information after analysis on all of the exploded nodes at this
   program point.

   __analyzer_dump_exploded_nodes (0);
   will dump just the number of nodes, and their IDs.

   __analyzer_dump_exploded_nodes (1);
   will also dump all of the states within those nodes.  */
extern void
__analyzer_dump_exploded_nodes (int)
  __attribute__ ((nothrow));

/* Emit a warning describing what is known about the value of NAME.  */
extern void
__analyzer_dump_named_constant (const char *name)
  __attribute__ ((nothrow));

/* Emit a placeholder "note" diagnostic with a path to this call site,
   if the analyzer finds a feasible path to it.  */
extern void
__analyzer_dump_path (void)
  __attribute__ ((nothrow));

/* Dump the region_model's state to stderr.  */
extern void
__analyzer_dump_region_model (void)
  __attribute__ ((nothrow));

/* Emit a warning describing the state of the 2nd argument
   (which can be of any type) with respect to NAME.
   This is for use when debugging, and may be of use in DejaGnu tests.  */
extern void
__analyzer_dump_state (const char *name, ...)
  __attribute__ ((nothrow));

/* Dump copious information about the analyzer’s state when reached.  */
extern void
__analyzer_dump_xml (void)
  __attribute__ ((nothrow));

/* Emit a warning with text "TRUE", FALSE" or "UNKNOWN" based on the
   truthfulness of the argument.  */
extern void
__analyzer_eval (int)
  __attribute__ ((nothrow));

/* Obtain an "unknown" void *.  */
extern void *
__analyzer_get_unknown_ptr (void)
  __attribute__ ((nothrow));

/* Complain if PTR doesn't point to a null-terminated string.
   TODO: eventually get the strlen of the buffer (without the
   optimizer touching it).  */
extern __SIZE_TYPE__
__analyzer_get_strlen (const char *ptr)
  __attribute__ ((nothrow));

#endif /* #ifndef ANALYZER_DECLS_H.  */
