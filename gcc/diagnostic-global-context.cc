/* Language-independent diagnostic subroutines that implicitly use global_dc.
   Copyright (C) 1999-2024 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This file implements the parts of the language independent aspect
   of diagnostic messages that implicitly use global_dc.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "diagnostic.h"
#include "diagnostic-format.h"

/* A diagnostic_context surrogate for stderr.  */
static diagnostic_context global_diagnostic_context;
diagnostic_context *global_dc = &global_diagnostic_context;

/* Standard error reporting routines in increasing order of severity.  */

/* Text to be emitted verbatim to the error message stream; this
   produces no prefix and disables line-wrapping.  Use rarely.
   It is ignored for machine-readable output formats.  */
void
verbatim (const char *gmsgid, ...)
{
  va_list ap;

  va_start (ap, gmsgid);
  text_info text (_(gmsgid), &ap, errno);
  global_dc->report_verbatim (text);
  va_end (ap);
}

/* Wrapper around diagnostic_context::diagnostic_impl
   implying global_dc and taking a variable argument list.  */

bool
emit_diagnostic (diagnostic_t kind,
		 location_t location,
		 diagnostic_option_id option_id,
		 const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_id,
					 gmsgid, &ap, kind);
  va_end (ap);
  return ret;
}

/* As above, but for rich_location *.  */

bool
emit_diagnostic (diagnostic_t kind,
		 rich_location *richloc,
		 diagnostic_option_id option_id,
		 const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = global_dc->diagnostic_impl (richloc, nullptr, option_id,
					 gmsgid, &ap, kind);
  va_end (ap);
  return ret;
}

/* As above, but taking a variable argument list.  */

bool
emit_diagnostic_valist (diagnostic_t kind,
			location_t location,
			diagnostic_option_id option_id,
			const char *gmsgid, va_list *ap)
{
  rich_location richloc (line_table, location);
  return global_dc->diagnostic_impl (&richloc, nullptr, option_id,
				     gmsgid, ap, kind);
}

/* As above, but with rich_location and metadata.  */

bool
emit_diagnostic_valist_meta (diagnostic_t kind,
			     rich_location *richloc,
			     const diagnostic_metadata *metadata,
			     diagnostic_option_id option_id,
			     const char *gmsgid, va_list *ap)
{
  return global_dc->diagnostic_impl (richloc, metadata, option_id,
				     gmsgid, ap, kind);
}

/* An informative note at LOCATION.  Use this for additional details on an error
   message.  */
void
inform (location_t location, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_NOTE);
  va_end (ap);
}

/* Same as "inform" above, but at RICHLOC.  */
void
inform (rich_location *richloc, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  global_dc->diagnostic_impl (richloc, nullptr, -1, gmsgid, &ap, DK_NOTE);
  va_end (ap);
}

/* An informative note at LOCATION.  Use this for additional details on an
   error message.  */
void
inform_n (location_t location, unsigned HOST_WIDE_INT n,
	  const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  va_list ap;
  va_start (ap, plural_gmsgid);
  auto_diagnostic_group d;
  rich_location richloc (line_table, location);
  global_dc->diagnostic_n_impl (&richloc, nullptr, -1, n,
				singular_gmsgid, plural_gmsgid,
				&ap, DK_NOTE);
  va_end (ap);
}

/* A warning at INPUT_LOCATION.  Use this for code which is correct according
   to the relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */
bool
warning (diagnostic_option_id option_id, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_id,
					 gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* A warning at LOCATION.  Use this for code which is correct according to the
   relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */

bool
warning_at (location_t location,
	    diagnostic_option_id option_id,
	    const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_id,
					 gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* Same as "warning at" above, but using RICHLOC.  */

bool
warning_at (rich_location *richloc,
	    diagnostic_option_id option_id,
	    const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = global_dc->diagnostic_impl (richloc, nullptr, option_id,
					 gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* Same as "warning at" above, but using METADATA.  */

bool
warning_meta (rich_location *richloc,
	      const diagnostic_metadata &metadata,
	      diagnostic_option_id option_id,
	      const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = global_dc->diagnostic_impl (richloc, &metadata, option_id,
					 gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* Same as warning_n plural variant below, but using RICHLOC.  */

bool
warning_n (rich_location *richloc,
	   diagnostic_option_id option_id,
	   unsigned HOST_WIDE_INT n,
	   const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, plural_gmsgid);
  bool ret = global_dc->diagnostic_n_impl (richloc, nullptr, option_id, n,
					   singular_gmsgid, plural_gmsgid,
					   &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* A warning at LOCATION.  Use this for code which is correct according to the
   relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */

bool
warning_n (location_t location,
	   diagnostic_option_id option_id,
	   unsigned HOST_WIDE_INT n,
	   const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, plural_gmsgid);
  rich_location richloc (line_table, location);
  bool ret = global_dc->diagnostic_n_impl (&richloc, nullptr, option_id, n,
					   singular_gmsgid, plural_gmsgid,
					   &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* A "pedantic" warning at LOCATION: issues a warning unless
   -pedantic-errors was given on the command line, in which case it
   issues an error.  Use this for diagnostics required by the relevant
   language standard, if you have chosen not to make them errors.

   Note that these diagnostics are issued independent of the setting
   of the -Wpedantic command-line switch.  To get a warning enabled
   only with that switch, use either "if (pedantic) pedwarn
   (OPT_Wpedantic,...)" or just "pedwarn (OPT_Wpedantic,..)".  To get a
   pedwarn independently of the -Wpedantic switch use "pedwarn (0,...)".

   Returns true if the warning was printed, false if it was inhibited.  */

bool
pedwarn (location_t location,
	 diagnostic_option_id option_id,
	 const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_id,
					 gmsgid, &ap, DK_PEDWARN);
  va_end (ap);
  return ret;
}

/* Same as pedwarn above, but using RICHLOC.  */

bool
pedwarn (rich_location *richloc,
	 diagnostic_option_id option_id,
	 const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = global_dc->diagnostic_impl (richloc, nullptr, option_id,
					 gmsgid, &ap, DK_PEDWARN);
  va_end (ap);
  return ret;
}

/* A "permissive" error at LOCATION: issues an error unless
   -fpermissive was given on the command line, in which case it issues
   a warning.  Use this for things that really should be errors but we
   want to support legacy code.

   Returns true if the warning was printed, false if it was inhibited.  */

bool
permerror (location_t location, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap,
					 DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* Same as "permerror" above, but at RICHLOC.  */

bool
permerror (rich_location *richloc, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = global_dc->diagnostic_impl (richloc, nullptr, -1, gmsgid, &ap,
					 DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* Similar to the above, but controlled by a flag other than -fpermissive.
   As above, an error by default or a warning with -fpermissive, but this
   diagnostic can also be downgraded by -Wno-error=opt.  */

bool
permerror_opt (location_t location,
	       diagnostic_option_id option_id,
	       const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_id,
					 gmsgid, &ap, DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* Same as "permerror" above, but at RICHLOC.  */

bool
permerror_opt (rich_location *richloc,
	       diagnostic_option_id option_id,
	       const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = global_dc->diagnostic_impl (richloc, nullptr, option_id,
					 gmsgid, &ap, DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* A hard error: the code is definitely ill-formed, and an object file
   will not be produced.  */
void
error (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* A hard error: the code is definitely ill-formed, and an object file
   will not be produced.  */
void
error_n (location_t location, unsigned HOST_WIDE_INT n,
	 const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, plural_gmsgid);
  rich_location richloc (line_table, location);
  global_dc->diagnostic_n_impl (&richloc, nullptr, -1, n,
				singular_gmsgid, plural_gmsgid,
				&ap, DK_ERROR);
  va_end (ap);
}

/* Same as above, but use location LOC instead of input_location.  */
void
error_at (location_t loc, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, loc);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* Same as above, but use RICH_LOC.  */

void
error_at (rich_location *richloc, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  global_dc->diagnostic_impl (richloc, nullptr, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* Same as above, but with metadata.  */

void
error_meta (rich_location *richloc, const diagnostic_metadata &metadata,
	    const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  global_dc->diagnostic_impl (richloc, &metadata, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* "Sorry, not implemented."  Use for a language feature which is
   required by the relevant specification but not implemented by GCC.
   An object file will not be produced.  */
void
sorry (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_SORRY);
  va_end (ap);
}

/* Same as above, but use location LOC instead of input_location.  */
void
sorry_at (location_t loc, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, loc);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_SORRY);
  va_end (ap);
}

/* Return true if an error or a "sorry" has been seen on global_dc.  Various
   processing is disabled after errors.  */
bool
seen_error (void)
{
  return errorcount || sorrycount;
}

/* An error which is severe enough that we make no attempt to
   continue.  Do not use this for internal consistency checks; that's
   internal_error.  Use of this function should be rare.  */
void
fatal_error (location_t loc, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, loc);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_FATAL);
  va_end (ap);

  gcc_unreachable ();
}

/* An internal consistency check has failed.  We make no attempt to
   continue.  */
void
internal_error (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_ICE);
  va_end (ap);

  gcc_unreachable ();
}

/* Like internal_error, but no backtrace will be printed.  Used when
   the internal error does not happen at the current location, but happened
   somewhere else.  */
void
internal_error_no_backtrace (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  global_dc->diagnostic_impl (&richloc, nullptr, -1, gmsgid, &ap, DK_ICE_NOBT);
  va_end (ap);

  gcc_unreachable ();
}


/* Special case error functions.  Most are implemented in terms of the
   above, or should be.  */

/* Print a diagnostic MSGID on FILE.  This is just fprintf, except it
   runs its second argument through gettext.  */
void
fnotice (FILE *file, const char *cmsgid, ...)
{
  /* If the user requested one of the machine-readable diagnostic output
     formats on stderr (e.g. -fdiagnostics-format=sarif-stderr), then
     emitting free-form text on stderr will lead to corrupt output.
     Skip the message for such cases.  */
  if (file == stderr && global_dc)
    if (!global_dc->supports_fnotice_on_stderr_p ())
      return;

  va_list ap;

  va_start (ap, cmsgid);
  vfprintf (file, _(cmsgid), ap);
  va_end (ap);
}

/* class auto_diagnostic_group.  */

/* Constructor: "push" this group into global_dc.  */

auto_diagnostic_group::auto_diagnostic_group ()
{
  global_dc->begin_group ();
}

/* Destructor: "pop" this group from global_dc.  */

auto_diagnostic_group::~auto_diagnostic_group ()
{
  global_dc->end_group ();
}

/* class auto_diagnostic_nesting_level.  */

auto_diagnostic_nesting_level::auto_diagnostic_nesting_level ()
{
  global_dc->push_nesting_level ();
}

auto_diagnostic_nesting_level::~auto_diagnostic_nesting_level ()
{
  global_dc->pop_nesting_level ();
}
