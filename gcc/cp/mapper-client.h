/* C++ modules.  Experimental!	-*- c++-mode -*-
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

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

/* Mapper to query and inform of modular compilations.  This is a
   singleton.  It contains both FILE and fd entities.  The PEX
   interface provides the former, so we need to keep them around.
   the fd entities are used when networking is supported.  */

class module_mapper {
  const char *name;
  FILE *from;   /* Read from mapper.  */
  pex_obj *pex; /* If it's a subprocess.  */
  sighandler_t sigpipe; /* Original sigpipe disposition.  */

  char *buffer; /* Line buffer.  */
  size_t size;  /* Allocated size of buffer.  */
  char *pos;	/* Read/Write point in buffer.  */
  char *end;	/* Ending NUL byte.  */
  char *start;  /* Start of current response line.  */
  int fd_from;	/* Fileno from mapper. */
  int fd_to;	/* Fileno to mapper. */
  bool batching;/* Batching requests or responses.  */

public:
  module_mapper ()
    : name (NULL), from (NULL), pex (NULL), sigpipe (SIG_IGN),
      buffer (NULL), size (0), pos (NULL), end (NULL),
      start (NULL), fd_from (-1), fd_to (-1), batching (false)
  {
  }
  ~module_mapper ()
  {
    gcc_assert (!from);
  }

public:
  const char *open (location_t loc, const char *connection, const char *dflt);
  void close (location_t loc);

public:

public:
  bool is_live () const
  {
    return fd_from >= 0;
  }
  bool is_server () const
  {
    return is_live () && fd_to >= 0;
  }
  bool is_file () const
  {
    return is_live () && fd_to < 0;
  }
  void close_file ()
  {
    gcc_checking_assert (is_file ());
    fclose (from);
    from = NULL;
    /* Leave fd_from alone to show liveness.  */
  }
  char const *get_name () const
  {
    return name;
  }

public:
  bool cork ()
  {
    batching = true;
    return batching;
  }
  void uncork (location_t loc)
  {
    if (batching)
      {
	batching = false;
	/* Need to disable gnu-printf zero-length format warning.  */
	send_command (loc, "%s", "");
      }
  }
  bool is_corked () const
  {
    return batching;
  }
  bool eol_p () const
  {
    return pos == end;
  }

public:
  void imex_query (location_t loc, const char *flatname, bool exporting);
  const char *imex_response (location_t loc, const char *flatname);
  bool translate_include (location_t, const char *path);

public:
  /* After a response that may be corked, eat blank lines until it is
     uncorked.  */
  void maybe_uncork (location_t loc)
  {
    while (is_corked ())
      if (get_response (loc) > 0)
	response_unexpected (loc);
  }

public:
  bool handshake (location_t, const char *main_src, char **repo);
  void send_command (location_t, const char * = NULL, ...) ATTRIBUTE_PRINTF_3;
  int get_response (location_t);
  char *response_token (location_t, bool all = false);
  int response_word (location_t, const char *, ...);
  const char *response_error ()
  {
    const char *result = pos != end ? pos : "unspecified error";
    pos = end;
    return result;
  }
  void response_unexpected (location_t);
  bool response_eol (location_t, bool ignore = false);
  const char *cmi_response (location_t, const char *);
};

