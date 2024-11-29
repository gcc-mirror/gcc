/* A program for re-emitting diagnostics saved in SARIF form.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "intl.h"
#include "libgdiagnostics++.h"
#include "libsarifreplay.h"

static const char *progname;

static void
set_defaults (replay_options &replay_opts)
{
  /* Defaults.  */
  replay_opts.m_echo_file = false;
  replay_opts.m_json_comments = false;
  replay_opts.m_verbose = false;
  replay_opts.m_diagnostics_colorize = DIAGNOSTIC_COLORIZE_IF_TTY;
}

struct options
{
  options ()
  {
    set_defaults (m_replay_opts);
  }

  replay_options m_replay_opts;
  std::vector<const char *> m_sarif_filenames;
};

static void
print_version ()
{
  printf (_("%s %s%s\n"), progname, pkgversion_string,
	  version_string);
  printf ("Copyright %s 2024 Free Software Foundation, Inc.\n",
	  _("(C)"));
  fputs (_("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"),
	 stdout);
}

static const char *const usage_msg = (
"sarif-replay [OPTIONS] FILE+\n"
"\n"
"  \"Replay\" results from one or more .sarif files as if they were\n"
"  GCC diagnostics\n"
"\n"
"Options:\n"
"\n"
"  -fdiagnostics-color={never|always|auto}\n"
"     Control colorization of diagnostics.  Default: auto.\n"
"\n"
"  -fjson-comments\n"
"     Support C and C++ style comments in .sarif JSON files\n"
"\n"
"  --verbose\n"
"     Print notes about each .sarif file before and after replaying it.\n"
"\n"
"  --echo-file\n"
"     Print the filename and file contents to stderr before replaying it.\n"
"\n"
"  -v, --version\n"
"     Print version and exit.\n"
"\n"
"  --usage\n"
"     Print this message and exit.\n"
"\n");

static void
print_usage ()
{
  fprintf (stderr, usage_msg);
}

static bool
parse_options (int argc, char **argv,
	       options &opts,
	       libgdiagnostics::text_sink control_text_sink)
{
  libgdiagnostics::manager options_mgr;
  options_mgr.set_tool_name ("sarif-replay");
  options_mgr.add_text_sink (stderr, DIAGNOSTIC_COLORIZE_NO/*IF_TTY*/);

  for (int i = 1; i < argc; ++i)
    {
      const char *option = argv[i];
      bool handled = false;
      if (strcmp (option, "-fjson-comments") == 0)
	{
	  opts.m_replay_opts.m_json_comments = true;
	  handled = true;
	}
      else if (strcmp (option, "--verbose") == 0)
	{
	  opts.m_replay_opts.m_verbose = true;
	  handled = true;
	}
      else if (strcmp (option, "--usage") == 0)
	{
	  print_usage ();
	  exit (0);
	}
      else if (strcmp (option, "--echo-file") == 0)
	{
	  opts.m_replay_opts.m_echo_file = true;
	  handled = true;
	}
      else if (strcmp (option, "-fdiagnostics-color=never") == 0)
	{
	  opts.m_replay_opts.m_diagnostics_colorize = DIAGNOSTIC_COLORIZE_NO;
	  control_text_sink.set_colorize
	    (opts.m_replay_opts.m_diagnostics_colorize);
	  handled = true;
	}
      else if (strcmp (option, "-fdiagnostics-color=always") == 0)
	{
	  opts.m_replay_opts.m_diagnostics_colorize = DIAGNOSTIC_COLORIZE_YES;
	  control_text_sink.set_colorize
	    (opts.m_replay_opts.m_diagnostics_colorize);
	  handled = true;
	}
      else if (strcmp (option, "-fdiagnostics-color=auto") == 0)
	{
	  opts.m_replay_opts.m_diagnostics_colorize
	    = DIAGNOSTIC_COLORIZE_IF_TTY;
	  control_text_sink.set_colorize
	    (opts.m_replay_opts.m_diagnostics_colorize);
	  handled = true;
	}
      else if (strcmp (option, "-v") == 0
	       || strcmp (option, "--version") == 0)
	{
	  print_version ();
	  exit (0);
	}

      if (!handled)
	{
	  if (option[0] == '-')
	    {
	      auto err = options_mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR);
	      err.finish ("unrecognized option: %qs", option);
	      return false;
	    }
	  else
	    opts.m_sarif_filenames.push_back (option);
	}
    }

  if (opts.m_sarif_filenames.size () == 0)
    {
      auto err = options_mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR);
      err.finish ("need at least one .sarif file to dump");
      return false;
    }
  return true;
}

static const char *
get_progname (const char *argv0)
{
  const char *p = argv0 + strlen (argv0);
  while (p != argv0 && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  return p;
}

/* Entrypoint to sarif-replay command-line tool.  */

int
main (int argc, char **argv)
{
  progname = get_progname (argv[0]);
  xmalloc_set_program_name (progname);

  libgdiagnostics::manager control_mgr;

  control_mgr.set_tool_name (progname);

  libgdiagnostics::text_sink control_text_sink
    = control_mgr.add_text_sink (stderr, DIAGNOSTIC_COLORIZE_IF_TTY);

  options opts;
  if (!parse_options (argc, argv, opts, control_text_sink))
    {
      print_usage ();
      return -1;
    }

  int failures = 0;
  for (auto filename : opts.m_sarif_filenames)
    {
      if (opts.m_replay_opts.m_verbose)
	{
	  auto note = control_mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_NOTE);
	  note.finish ("about to replay %qs...", filename);
	}
      libgdiagnostics::manager playback_mgr;
      playback_mgr.add_text_sink (stderr,
				  opts.m_replay_opts.m_diagnostics_colorize);

      int result = sarif_replay_path (filename,
				      playback_mgr.m_inner,
				      control_mgr.m_inner,
				      &opts.m_replay_opts);
      if (result)
	++failures;

      if (opts.m_replay_opts.m_verbose)
	{
	  auto note = control_mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_NOTE);
	  note.finish ("...finished replaying %qs", filename);
	}
    }
  return failures;
}
