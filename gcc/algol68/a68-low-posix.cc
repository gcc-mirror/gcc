/* Lowering routines for the POSIX prelude.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Number of command line arguments passed to the program.  */

tree
a68_posix_argc (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_ARGC);
}

/* Gets the Nth command line argument passed to the program.  If N is out of
   range the result is an empty string.  */

tree
a68_posix_argv (void)
{
  static tree argv_fndecl;

  if (argv_fndecl == NULL_TREE)
    {
      argv_fndecl
	= a68_low_toplevel_func_decl ("argv",
				      build_function_type_list (CTYPE (M_STRING),
								a68_int_type,
								NULL_TREE));
      announce_function (argv_fndecl);

      tree param = a68_low_func_param (argv_fndecl, "n", a68_int_type);
      DECL_ARGUMENTS (argv_fndecl) = param;

      a68_push_function_range (argv_fndecl, CTYPE (M_STRING),
			       true /* top_level */);

      a68_push_range (M_STRING);
      tree len = a68_lower_tmpvar ("len%", sizetype, size_int (0));
      TREE_ADDRESSABLE (len) = 1;

      tree ptrtochar_type = build_pointer_type (a68_char_type);
      tree elems = a68_lower_tmpvar ("elems%", ptrtochar_type,
				     a68_build_libcall (A68_LIBCALL_POSIX_ARGV,
							ptrtochar_type, 2,
							param,
							fold_build1 (ADDR_EXPR, build_pointer_type (sizetype),
								     len)));
      tree lower_bound = ssize_int (1);
      tree upper_bound = fold_convert (ssizetype, len);
      tree elems_size = fold_build2 (MULT_EXPR, sizetype,
				     len,
				     size_in_bytes (a68_char_type));
      a68_add_stmt (a68_row_value (CTYPE (M_STRING), 1 /* dim */,
				   elems, elems_size,
				   &lower_bound, &upper_bound));
      tree body = a68_pop_range ();
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (argv_fndecl)),
		      argv_fndecl);
}

/* Gets the value of an environment variable, or an empty string if the
   variable is not set.  */

tree
a68_posix_getenv (void)
{
  static tree getenv_fndecl;

  if (getenv_fndecl == NULL_TREE)
    {
      getenv_fndecl
	= a68_low_toplevel_func_decl ("getenv",
				      build_function_type_list (CTYPE (M_STRING),
								CTYPE (M_STRING),
								NULL_TREE));
      announce_function (getenv_fndecl);

      tree param = a68_low_func_param (getenv_fndecl, "varname", CTYPE (M_STRING));
      DECL_ARGUMENTS (getenv_fndecl) = param;

      a68_push_function_range (getenv_fndecl, CTYPE (M_STRING),
			       true /* top_level */);

      a68_push_range (M_STRING);

      tree varname = a68_lower_tmpvar ("varname%", CTYPE (M_STRING),
				       param);

      tree ptrtochar_type = build_pointer_type (a68_char_type);
      tree convelems = a68_lower_tmpvar ("convelems%", ptrtochar_type,
					 build_int_cst (ptrtochar_type, 0));
      TREE_ADDRESSABLE (convelems) = 1;
      tree convelemslen = a68_lower_tmpvar ("convelemslen%", sizetype,
					    size_int (0));
      TREE_ADDRESSABLE (convelemslen) = 1;

      tree call = a68_build_libcall (A68_LIBCALL_POSIX_GETENV,
				     void_type_node, 5,
				     a68_multiple_elements (varname),
				     a68_multiple_num_elems (varname),
				     a68_multiple_stride (varname, size_zero_node),
				     fold_build1 (ADDR_EXPR, build_pointer_type (ptrtochar_type),
						  convelems),
				     fold_build1 (ADDR_EXPR, build_pointer_type (sizetype),
						  convelemslen));
      a68_add_stmt (call);
      tree lower_bound = ssize_int (1);
      tree upper_bound = fold_convert (ssizetype, convelemslen);
      tree convelems_size = fold_build2 (MULT_EXPR, sizetype,
					 convelemslen,
					 size_in_bytes (a68_char_type));
      a68_add_stmt (a68_row_value (CTYPE (M_STRING), 1 /* dim */,
				   convelems, convelems_size,
				   &lower_bound, &upper_bound));
      tree body = a68_pop_range ();
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (getenv_fndecl)),
		      getenv_fndecl);
}

tree
a68_posix_putchar (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_PUTCHAR);
}

tree
a68_posix_puts (void)
{
  static tree puts_fndecl;

  if (puts_fndecl == NULL_TREE)
    {
      puts_fndecl
	= a68_low_toplevel_func_decl ("puts",
				      build_function_type_list (void_type_node,
								CTYPE (M_STRING),
								NULL_TREE));
      announce_function (puts_fndecl);

      tree param = a68_low_func_param (puts_fndecl, "str", CTYPE (M_STRING));
      DECL_ARGUMENTS (puts_fndecl) = param;

      a68_push_function_range (puts_fndecl, void_type_node,
			       true /* top_level */);

      tree call = a68_build_libcall (A68_LIBCALL_POSIX_PUTS,
				     void_type_node, 3,
				     a68_multiple_elements (param),
				     a68_multiple_num_elems (param),
				     a68_multiple_stride (param, size_zero_node));
      a68_pop_function_range (call);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (puts_fndecl)),
		      puts_fndecl);
}

tree
a68_posix_fconnect (void)
{
  static tree fconnect_fndecl;

  if (fconnect_fndecl == NULL_TREE)
    {
      fconnect_fndecl
	= a68_low_toplevel_func_decl ("fconnect",
				      build_function_type_list (a68_int_type,
								CTYPE (M_STRING),
								a68_bits_type,
								NULL_TREE));
      announce_function (fconnect_fndecl);

      tree host = a68_low_func_param (fconnect_fndecl, "host", CTYPE (M_STRING));
      tree port = a68_low_func_param (fconnect_fndecl, "port", a68_int_type);
      DECL_ARGUMENTS (fconnect_fndecl) = chainon (host, port);

      a68_push_function_range (fconnect_fndecl, a68_int_type,
			       true /* top_level */);


      tree body = a68_build_libcall (A68_LIBCALL_POSIX_FCONNECT,
				     a68_int_type, 4,
				     a68_multiple_elements (host),
				     a68_multiple_num_elems (host),
				     a68_multiple_stride (host, size_zero_node),
				     port);
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fconnect_fndecl)),
		      fconnect_fndecl);
}

tree
a68_posix_fcreate (void)
{
  static tree fcreate_fndecl;

  if (fcreate_fndecl == NULL_TREE)
    {
      fcreate_fndecl
	= a68_low_toplevel_func_decl ("fcreate",
				      build_function_type_list (a68_int_type,
								CTYPE (M_STRING),
								a68_bits_type,
								NULL_TREE));
      announce_function (fcreate_fndecl);

      tree pathname = a68_low_func_param (fcreate_fndecl, "pathname", CTYPE (M_STRING));
      tree mode = a68_low_func_param (fcreate_fndecl, "mode", a68_int_type);
      DECL_ARGUMENTS (fcreate_fndecl) = chainon (pathname, mode);

      a68_push_function_range (fcreate_fndecl, a68_int_type,
			       true /* top_level */);


      tree body = a68_build_libcall (A68_LIBCALL_POSIX_FCREATE,
				     a68_int_type, 4,
				     a68_multiple_elements (pathname),
				     a68_multiple_num_elems (pathname),
				     a68_multiple_stride (pathname, size_zero_node),
				     mode);
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fcreate_fndecl)),
		      fcreate_fndecl);
}

tree
a68_posix_fopen (void)
{
  static tree fopen_fndecl;

  if (fopen_fndecl == NULL_TREE)
    {
      fopen_fndecl
	= a68_low_toplevel_func_decl ("fopen",
				      build_function_type_list (a68_int_type,
								CTYPE (M_STRING),
								a68_bits_type,
								NULL_TREE));
      announce_function (fopen_fndecl);

      tree pathname = a68_low_func_param (fopen_fndecl, "pathname", CTYPE (M_STRING));
      tree flags = a68_low_func_param (fopen_fndecl, "flags", a68_int_type);
      DECL_ARGUMENTS (fopen_fndecl) = chainon (pathname, flags);

      a68_push_function_range (fopen_fndecl, a68_int_type,
			       true /* top_level */);


      tree body = a68_build_libcall (A68_LIBCALL_POSIX_FOPEN,
				     a68_int_type, 4,
				     a68_multiple_elements (pathname),
				     a68_multiple_num_elems (pathname),
				     a68_multiple_stride (pathname, size_zero_node),
				     flags);
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fopen_fndecl)),
		      fopen_fndecl);
}

tree
a68_posix_fclose (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_FCLOSE);
}

tree
a68_posix_fsize (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_FSIZE);
}

tree
a68_posix_lseek (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_LSEEK);
}

tree
a68_posix_errno (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_ERRNO);
}

tree
a68_posix_exit (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_EXIT);
}

tree
a68_posix_perror (void)
{
  static tree perror_fndecl;

  if (perror_fndecl == NULL_TREE)
    {
      perror_fndecl
	= a68_low_toplevel_func_decl ("perror",
				      build_function_type_list (void_type_node,
								CTYPE (M_STRING),
								NULL_TREE));
      announce_function (perror_fndecl);

      tree str = a68_low_func_param (perror_fndecl, "str", CTYPE (M_STRING));
      DECL_ARGUMENTS (perror_fndecl) = str;

      a68_push_function_range (perror_fndecl, void_type_node,
			       true /* top_level */);

      tree body = a68_build_libcall (A68_LIBCALL_POSIX_PERROR,
				     a68_int_type, 3,
				     a68_multiple_elements (str),
				     a68_multiple_num_elems (str),
				     a68_multiple_stride (str, size_zero_node));
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (perror_fndecl)),
		      perror_fndecl);
}

tree
a68_posix_strerror (void)
{
  static tree strerror_fndecl;

  if (strerror_fndecl == NULL_TREE)
    {
      strerror_fndecl
	= a68_low_toplevel_func_decl ("strerror",
				      build_function_type_list (CTYPE (M_STRING),
								a68_int_type,
								NULL_TREE));
      announce_function (strerror_fndecl);

      tree errnum = a68_low_func_param (strerror_fndecl, "errnum", a68_int_type);
      DECL_ARGUMENTS (strerror_fndecl) = errnum;

      a68_push_function_range (strerror_fndecl, CTYPE (M_STRING),
			       true /* top_level */);

      tree len = a68_lower_tmpvar ("len%", sizetype, size_int (0));
      TREE_ADDRESSABLE (len) = 1;

      tree call = a68_build_libcall (A68_LIBCALL_POSIX_STRERROR,
				     void_type_node, 2,
				     errnum,
				     fold_build1 (ADDR_EXPR, build_pointer_type (sizetype), len));
      tree elems = a68_lower_tmpvar ("elems%", build_pointer_type (a68_char_type), call);

      tree lower_bound = ssize_int (1);
      tree upper_bound = fold_convert (ssizetype, len);
      tree elems_size = fold_build2 (MULT_EXPR, sizetype,
				     len, size_in_bytes (a68_char_type));

      tree body = a68_row_value (CTYPE (M_STRING), 1 /* dim */,
				 elems, elems_size,
				 &lower_bound, &upper_bound);
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (strerror_fndecl)),
		      strerror_fndecl);
}

tree
a68_posix_getchar (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_GETCHAR);
}

tree
a68_posix_fgetc (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_FGETC);
}

tree
a68_posix_fputc (void)
{
  return a68_get_libcall (A68_LIBCALL_POSIX_FPUTC);
}

tree
a68_posix_fputs (void)
{
  static tree fputs_fndecl;

  if (fputs_fndecl == NULL_TREE)
    {
      fputs_fndecl
	= a68_low_toplevel_func_decl ("fputs",
				      build_function_type_list (a68_int_type,
								a68_int_type,
								CTYPE (M_STRING),
								NULL_TREE));
      announce_function (fputs_fndecl);

      tree fd = a68_low_func_param (fputs_fndecl, "fd", a68_int_type);
      tree str = a68_low_func_param (fputs_fndecl, "str", CTYPE (M_STRING));
      DECL_ARGUMENTS (fputs_fndecl) = chainon (fd, str);

      a68_push_function_range (fputs_fndecl, a68_int_type,
			       true /* top_level */);


      tree body = a68_build_libcall (A68_LIBCALL_POSIX_FPUTS,
				     a68_int_type, 4,
				     fd,
				     a68_multiple_elements (str),
				     a68_multiple_num_elems (str),
				     a68_multiple_stride (str, size_zero_node));
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fputs_fndecl)),
		      fputs_fndecl);
}

tree
a68_posix_fgets (void)
{
  static tree fgets_fndecl;

  if (fgets_fndecl == NULL_TREE)
    {
      fgets_fndecl
	= a68_low_toplevel_func_decl ("fgets",
				      build_function_type_list (CTYPE (M_REF_STRING),
								a68_int_type,
								a68_int_type,
								NULL_TREE));
      announce_function (fgets_fndecl);

      tree fd = a68_low_func_param (fgets_fndecl, "fd", a68_int_type);
      tree n = a68_low_func_param (fgets_fndecl, "n", a68_int_type);
      DECL_ARGUMENTS (fgets_fndecl) = chainon (fd, n);

      a68_push_function_range (fgets_fndecl, CTYPE (M_REF_STRING),
			       true /* top_level */);

      tree len = a68_lower_tmpvar ("len%", sizetype, size_int (0));
      TREE_ADDRESSABLE (len) = 1;

      tree call = a68_build_libcall (A68_LIBCALL_POSIX_FGETS,
				     CTYPE (M_REF_STRING), 3,
				     fd, n,
				     fold_build1 (ADDR_EXPR, build_pointer_type (sizetype), len));
      tree elems = a68_lower_tmpvar ("elems%", build_pointer_type (a68_char_type), call);

      tree lower_bound = ssize_int (1);
      tree upper_bound = fold_convert (ssizetype, len);
      tree elems_size = fold_build2 (MULT_EXPR, sizetype,
				     len, size_in_bytes (a68_char_type));
      tree body = a68_row_malloc (M_STRING, 1 /* dim */,
				  elems, elems_size,
				  &lower_bound, &upper_bound);
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fgets_fndecl)),
		      fgets_fndecl);
}

tree
a68_posix_gets (void)
{
  static tree gets_fndecl;

  if (gets_fndecl == NULL_TREE)
    {
      gets_fndecl
	= a68_low_toplevel_func_decl ("gets",
				      build_function_type_list (CTYPE (M_REF_STRING),
								a68_int_type,
								NULL_TREE));
      announce_function (gets_fndecl);

      tree n = a68_low_func_param (gets_fndecl, "n", a68_int_type);
      DECL_ARGUMENTS (gets_fndecl) = n;

      a68_push_function_range (gets_fndecl, CTYPE (M_REF_STRING),
			       true /* top_level */);

      tree len = a68_lower_tmpvar ("len%", sizetype, size_int (0));
      TREE_ADDRESSABLE (len) = 1;

      tree call = a68_build_libcall (A68_LIBCALL_POSIX_GETS,
				     CTYPE (M_REF_STRING), 2,
				     n, fold_build1 (ADDR_EXPR, build_pointer_type (sizetype), len));
      tree elems = a68_lower_tmpvar ("elems%", build_pointer_type (a68_char_type), call);

      tree lower_bound = ssize_int (1);
      tree upper_bound = fold_convert (ssizetype, len);
      tree elems_size = fold_build2 (MULT_EXPR, sizetype,
				     len, size_in_bytes (a68_char_type));
      tree body = a68_row_malloc (M_STRING, 1 /* dim */,
				  elems, elems_size,
				  &lower_bound, &upper_bound);
      a68_pop_function_range (body);
    }

  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (gets_fndecl)),
		      gets_fndecl);
}
