/* Handling of fnspec attribute specifiers
   Copyright (C) 2008-2020 Free Software Foundation, Inc.
   Contributed by Richard Guenther  <rguenther@suse.de>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Parse string of attribute "fn spec".  This is an internal attribute
   describing side effects of a function as follows:

   character 0  specifies properties of return values as follows:
     '1'...'4'  specifies number of argument function returns (as in memset)
     'm'	specifies that returned value is noalias (as in malloc)
     '.'	specifies that nothing is known.
   character 1  specifies additional function properties
     ' '        specifies that nothing is known

   character 2+2i specifies properties of argument number i as follows:
     'x' or 'X' specifies that parameter is unused.
     'r' or 'R' specifies that the memory pointed to by the parameter is only
		read and does not escape
     'w' or 'W' specifies that the memory pointed to by the parameter does not
		escape
     '.'	specifies that nothing is known.
   The uppercase letter in addition specifies that the memory pointed to
   by the parameter is not dereferenced.  For 'r' only read applies
   transitively to pointers read from the pointed-to memory.

   character 3+2i specifies additional properties of argument number i
   as follows:
     ' '        nothing is known
 */

#ifndef ATTR_FNSPEC_H
#define ATTR_FNSPEC_H

class attr_fnspec
{
private:
  /* fn spec attribute string.  */
  const char *str;
  /* length of the fn spec string.  */
  const unsigned len;
  /* Number of characters specifying return value.  */
  const unsigned int return_desc_size = 2;
  /* Number of characters specifying size.  */
  const unsigned int arg_desc_size = 2;

  /* Return start of specifier of arg i.  */
  unsigned int arg_idx (int i)
  {
    return return_desc_size + arg_desc_size * i;
  }

public:
  attr_fnspec (const char *str, unsigned len)
  : str (str), len (len)
  {
    if (flag_checking)
      verify ();
  }
  attr_fnspec (const_tree identifier)
  : str (TREE_STRING_POINTER (identifier)),
    len (TREE_STRING_LENGTH (identifier))
  {
    if (flag_checking)
      verify ();
  }

  /* Return true if arg I is specified.  */
  bool
  arg_specified_p (unsigned int i)
  {
    return len >= arg_idx (i + 1);
  }

  /* True if the argument is not dereferenced recursively, thus only
     directly reachable memory is read or written.  */
  bool
  arg_direct_p (unsigned int i)
  {
    unsigned int idx = arg_idx (i);
    gcc_checking_assert (arg_specified_p (i));
    return str[idx] == 'R' || str[idx] == 'W';
  }

  /* True if argument is used.  */
  bool
  arg_used_p (unsigned int i)
  {
    unsigned int idx = arg_idx (i);
    gcc_checking_assert (arg_specified_p (i));
    return str[idx] != 'x' && str[idx] != 'X';
  }

  /* True if memory reached by the argument is readonly (not clobbered).  */
  bool
  arg_readonly_p (unsigned int i)
  {
    unsigned int idx = arg_idx (i);
    gcc_checking_assert (arg_specified_p (i));
    return str[idx] == 'r' || str[idx] == 'R';
  }

  /* True if the argument does not escape.  */
  bool
  arg_noescape_p (unsigned int i)
  {
    unsigned int idx = arg_idx (i);
    gcc_checking_assert (arg_specified_p (i));
    return str[idx] == 'w' || str[idx] == 'W'
	   || str[idx] == 'R' || str[idx] == 'r';
  }

  /* Return true if function returns value of its parameter.  If ARG_NO is
     non-NULL return initialize it to the argument returned.  */
  bool
  returns_arg (unsigned int *arg_no)
  {
    if (str[0] >= '1' && str[0] <= '4')
      {
	if (arg_no)
	  *arg_no = str[0] - '1';
	return true;
      }
    return false;
  }

  /* Nonzero if the return value does not alias with anything.  Functions
     with the malloc attribute have this set on their return value.  */
  bool
  returns_noalias_p ()
  {
    return str[0] == 'm';
  }

  /* Check validity of the string.  */
  void verify ();
};

#endif /* ATTR_FNSPEC_H  */
