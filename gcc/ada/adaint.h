/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               A D A I N T                                *
 *                                                                          *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2002 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#if defined(__rtems__)
#include <stdio.h>
#endif

#include <dirent.h>

extern int    __gnat_max_path_len;
extern void   __gnat_to_gm_time			   PARAMS ((int *, int *,
							    int *, int *,
							    int *, int *,
							    int *));
extern int    __gnat_get_maximum_file_name_length  PARAMS ((void));
extern int    __gnat_get_switches_case_sensitive   PARAMS ((void));
extern int    __gnat_get_file_names_case_sensitive PARAMS ((void));
extern char   __gnat_get_default_identifier_character_set PARAMS ((void));
extern void   __gnat_get_current_dir		   PARAMS ((char *, int *));
extern void   __gnat_get_object_suffix_ptr         PARAMS ((int *,
							    const char **));
extern void   __gnat_get_executable_suffix_ptr     PARAMS ((int *,
							    const char **));
extern void   __gnat_get_debuggable_suffix_ptr     PARAMS ((int *,
							    const char **));
extern int    __gnat_readlink			   PARAMS ((char *, char *,
							    size_t));
extern int    __gnat_symlink                       PARAMS ((char *, char *));
extern int    __gnat_try_lock                      PARAMS ((char *, char *));
extern int    __gnat_open_new                      PARAMS ((char *, int));
extern int    __gnat_open_new_temp		   PARAMS ((char *, int));
extern int    __gnat_mkdir			   PARAMS ((char *));
extern int    __gnat_stat			   PARAMS ((char *, 
							    struct stat *));
extern int    __gnat_open_read                     PARAMS ((char *, int));
extern int    __gnat_open_rw                       PARAMS ((char *, int));
extern int    __gnat_open_create                   PARAMS ((char *, int));
extern int    __gnat_open_append                   PARAMS ((char *, int));
extern long   __gnat_file_length                   PARAMS ((int));
extern void   __gnat_tmp_name			   PARAMS ((char *));
extern char  *__gnat_readdir                       PARAMS ((DIR *, char *));
extern int    __gnat_readdir_is_thread_safe        PARAMS ((void));
extern time_t __gnat_file_time_name                PARAMS ((char *));
extern time_t __gnat_file_time_fd                  PARAMS ((int));
extern void   __gnat_set_file_time_name		   PARAMS ((char *, time_t));
extern void   __gnat_get_env_value_ptr             PARAMS ((char *, int *,
							    char **));
extern int    __gnat_file_exists		   PARAMS ((char *));
extern int    __gnat_is_regular_file               PARAMS ((char *));
extern int    __gnat_is_absolute_path              PARAMS ((char *));
extern int    __gnat_is_directory		   PARAMS ((char *));
extern int    __gnat_is_writable_file		   PARAMS ((char *));
extern int    __gnat_portable_spawn                PARAMS ((char *[]));
extern int    __gnat_portable_no_block_spawn       PARAMS ((char *[]));
extern int    __gnat_portable_wait                 PARAMS ((int *));
extern int    __gnat_waitpid			   PARAMS ((int));
extern char  *__gnat_locate_exec                   PARAMS ((char *, char *));
extern char  *__gnat_locate_exec_on_path		   PARAMS ((char *));
extern char  *__gnat_locate_regular_file           PARAMS ((char *, char *));
extern void   __gnat_maybe_glob_args               PARAMS ((int *, char ***));
extern void   __gnat_os_exit			   PARAMS ((int));
extern void   __gnat_set_env_value		   PARAMS ((char *, char *));
extern char  *__gnat_get_libraries_from_registry   PARAMS ((void));
extern int    __gnat_to_canonical_file_list_init   PARAMS ((char *, int));
extern char  *__gnat_to_canonical_file_list_next   PARAMS ((void));
extern void   __gnat_to_canonical_file_list_free   PARAMS ((void));
extern char  *__gnat_to_canonical_dir_spec         PARAMS ((char *, int));
extern char  *__gnat_to_canonical_file_spec        PARAMS ((char *));
extern char  *__gnat_to_host_dir_spec              PARAMS ((char *, int));
extern char  *__gnat_to_host_file_spec             PARAMS ((char *));
extern char  *__gnat_to_canonical_path_spec	   PARAMS ((char *));
extern void   __gnat_adjust_os_resource_limits	   PARAMS ((void));

extern int     __gnat_feof		  	   PARAMS ((FILE *));
extern int     __gnat_ferror		  	   PARAMS ((FILE *));
extern int     __gnat_fileno		  	   PARAMS ((FILE *));
extern int     __gnat_is_regular_file_fd  	   PARAMS ((int));
extern FILE *__gnat_constant_stderr	  	   PARAMS ((void));
extern FILE *__gnat_constant_stdin	  	   PARAMS ((void));
extern FILE *__gnat_constant_stdout	  	   PARAMS ((void));
extern char *__gnat_full_name		  	   PARAMS ((char *, char *));

extern int    __gnat_arg_count			   PARAMS ((void));
extern int    __gnat_len_arg			   PARAMS ((int));
extern void   __gnat_fill_arg			   PARAMS ((char *, int));
extern int    __gnat_env_count			   PARAMS ((void));
extern int    __gnat_len_env			   PARAMS ((int));
extern void   __gnat_fill_env			   PARAMS ((char *, int));

/* Routines for interface to scanf and printf functions for integer values */

extern int    get_int				   PARAMS ((void));
extern void   put_int				   PARAMS ((int));
extern void   put_int_stderr			   PARAMS ((int));
extern int    get_char				   PARAMS ((void));
extern void   put_char				   PARAMS ((int));
extern void   put_char_stderr			   PARAMS ((int));
extern char  *mktemp				   PARAMS ((char *));

extern void   __gnat_set_exit_status		   PARAMS ((int));

extern int    __gnat_expect_fork		   PARAMS ((void));
extern void   __gnat_expect_portable_execvp	   PARAMS ((char *, char *[]));
extern int    __gnat_pipe			   PARAMS ((int *));
extern int    __gnat_expect_poll		   PARAMS ((int *, int, int,
							    int *));
extern void    __gnat_set_binary_mode		   PARAMS ((int));
extern void    __gnat_set_text_mode		   PARAMS ((int));
extern char   *__gnat_ttyname			   PARAMS ((int));

#ifdef IN_RTS
/* Portable definition of strdup, which is not available on all systems.  */
#define xstrdup(S)  strcpy ((char *) malloc (strlen (S) + 1), S)
#endif
