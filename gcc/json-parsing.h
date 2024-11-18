/* JSON parsing
   Copyright (C) 2017-2022 Free Software Foundation, Inc.
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

#ifndef GCC_JSON_PARSING_H
#define GCC_JSON_PARSING_H

#include "json.h"

namespace json
{

/* Declarations for parsing JSON to a json::value * tree.  */

/* Abstract base class for recording what the locations of JSON values
   were as they parsed.  */

class location_map
{
public:
  /* A point within the JSON input file.  */
  struct point
  {
    size_t m_unichar_idx; /* zero-based.  */
    int m_line;   /* one-based.  */
    int m_column; /* zero-based unichar count.  */
  };

  /* A range of points within the JSON input file.
     Both endpoints are part of the range.  */
  struct range
  {
    point m_start;
    point m_end;
  };

  virtual ~location_map () {}
  virtual void record_range_for_value (json::value *jv, const range &r) = 0;
  virtual void on_finished_parsing () {}
};

/* Class for recording an error within a JSON file.  */

class error
{
public:
  error (const location_map::range &r, char *msg)
  : m_range (r), m_msg (msg)
  {
  }
  ~error ()
  {
    free (m_msg);
  }

  const location_map::range &get_range () const { return m_range; }
  const char *get_msg () const { return m_msg; }

private:
  location_map::range m_range;
  char *m_msg;
};

/* Class for the result of an operation: either a value or an error
   (or both null for the case of "successful nullptr").
   The types must be default-constructible.  */

template <typename ValueType, typename ErrorType>
struct result
{
  result (ValueType val) : m_val (std::move (val)), m_err () {}
  result (ErrorType err) : m_val (), m_err (std::move (err)) {}

  ValueType m_val;
  ErrorType m_err;
};

/* Typedef for the result of parsing JSON: ownership of either a
   json::value * or of a json::error *.  */
typedef result<std::unique_ptr<value>,
	       std::unique_ptr<error>> parser_result_t;

/* Functions for parsing JSON buffers.  */

extern parser_result_t
parse_utf8_string (size_t length,
		   const char *utf8_buf,
		   bool allow_comments,
		   location_map *out_loc_map);
extern parser_result_t
parse_utf8_string (const char *utf8,
		   bool allow_comments,
		   location_map *out_loc_map);

} // namespace json

#endif  /* GCC_JSON_PARSING_H  */
