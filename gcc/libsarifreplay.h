/* A pure C API for replaying SARIF as diagnostics.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef LIBSARIFREPLAY_H
#define LIBSARIFREPLAY_H

#include "libgdiagnostics.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct replay_options
{
  bool m_echo_file;
  bool m_json_comments;
  bool m_verbose;
  enum diagnostic_colorize m_diagnostics_colorize;
};

/* Attempt to load a .sarif file from SARIF_FILE, and
   replay the diagnostics to OUTPUT_MANAGER.
   Report any problems to CONTROL_MANAGER (such as
   file-not-found, malformed .sarif, etc).
   If ALLOW_JSON_COMMENTS is true, then allow C/C++ style comments
   in the file.
   If ECHO_FILE, then dump the filename and contents to stderr.  */

extern int
sarif_replay_path (const char *sarif_file,
		   diagnostic_manager *output_manager,
		   diagnostic_manager *control_manager,
		   const replay_options *options)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (4);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* LIBSARIFREPLAY_H  */
