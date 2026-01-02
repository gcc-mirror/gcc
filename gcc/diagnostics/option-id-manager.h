/* Hooks for giving client-specific meaning to option ids.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_OPTION_ID_MANAGER_H
#define GCC_DIAGNOSTICS_OPTION_ID_MANAGER_H

namespace diagnostics {

/* Abstract base class for the diagnostic subsystem to make queries
   about command-line options.  */

class option_id_manager
{
public:
  virtual ~option_id_manager () {}

  /* Return 1 if option OPT_ID is enabled, 0 if it is disabled,
     or -1 if it isn't a simple on-off switch
     (or if the value is unknown, typically set later in target).  */
  virtual int option_enabled_p (option_id opt_id) const = 0;

  /* Return malloced memory for the name of the option OPT_ID
     which enabled a diagnostic, originally of type ORIG_DIAG_KIND but
     possibly converted to DIAG_KIND by options such as -Werror.
     May return NULL if no name is to be printed.
     May be passed 0 as well as the index of a particular option.  */
  virtual char *make_option_name (option_id opt_id,
				  enum kind orig_diag_kind,
				  enum kind diag_kind) const = 0;

  /* Return malloced memory for a URL describing the option that controls
     a diagnostic.
     May return NULL if no URL is available.
     May be passed 0 as well as the index of a particular option.  */
  virtual char *make_option_url (option_id opt_id) const = 0;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_OPTION_ID_MANAGER_H */
