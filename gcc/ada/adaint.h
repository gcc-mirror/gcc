/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               A D A I N T                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2005 Free Software Foundation, Inc.          *
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

#include <stdio.h>
#include <dirent.h>

typedef long OS_Time; /* Type corresponding to GNAT.OS_Lib.OS_Time */

extern int    __gnat_max_path_len;
extern void   __gnat_to_gm_time			   (OS_Time *, int *,
						    int *, int *,
						    int *, int *,
						    int *);
extern int    __gnat_get_maximum_file_name_length  (void);
extern int    __gnat_get_switches_case_sensitive   (void);
extern int    __gnat_get_file_names_case_sensitive (void);
extern char   __gnat_get_default_identifier_character_set (void);
extern void   __gnat_get_current_dir		   (char *, int *);
extern void   __gnat_get_object_suffix_ptr         (int *,
						    const char **);
extern void   __gnat_get_executable_suffix_ptr     (int *,
						    const char **);
extern void   __gnat_get_debuggable_suffix_ptr     (int *,
						    const char **);
extern int    __gnat_readlink			   (char *, char *,
						    size_t);
extern int    __gnat_symlink                       (char *, char *);
extern int    __gnat_try_lock                      (char *, char *);
extern int    __gnat_open_new                      (char *, int);
extern int    __gnat_open_new_temp		   (char *, int);
extern int    __gnat_mkdir			   (char *);
extern int    __gnat_stat			   (char *,
						    struct stat *);
extern int    __gnat_open_read                     (char *, int);
extern int    __gnat_open_rw                       (char *, int);
extern int    __gnat_open_create                   (char *, int);
extern int    __gnat_create_output_file            (char *);
extern int    __gnat_open_append                   (char *, int);
extern long   __gnat_file_length                   (int);
extern long   __gnat_named_file_length             (char *);
extern void   __gnat_tmp_name			   (char *);
extern char  *__gnat_readdir                       (DIR *, char *);
extern int    __gnat_readdir_is_thread_safe        (void);

extern OS_Time __gnat_file_time_name                (char *);
extern OS_Time __gnat_file_time_fd                  (int);
/* return -1 in case of error */

extern void   __gnat_set_file_time_name		   (char *, time_t);
extern void   __gnat_get_env_value_ptr             (char *, int *,
						    char **);
extern int    __gnat_dup			   (int);
extern int    __gnat_dup2			   (int, int);
extern int    __gnat_file_exists		   (char *);
extern int    __gnat_is_regular_file               (char *);
extern int    __gnat_is_absolute_path              (char *,int);
extern int    __gnat_is_directory		   (char *);
extern int    __gnat_is_writable_file		   (char *);
extern int    __gnat_is_readable_file		   (char *name);
extern void   __gnat_set_readonly                  (char *name);
extern void   __gnat_set_writable                  (char *name);
extern void   __gnat_set_executable                (char *name);
extern int    __gnat_is_symbolic_link		   (char *name);
extern int    __gnat_portable_spawn                (char *[]);
extern int    __gnat_portable_no_block_spawn       (char *[]);
extern int    __gnat_portable_wait                 (int *);
extern int    __gnat_waitpid			   (int);
extern char  *__gnat_locate_exec                   (char *, char *);
extern char  *__gnat_locate_exec_on_path	   (char *);
extern char  *__gnat_locate_regular_file           (char *, char *);
extern void   __gnat_maybe_glob_args               (int *, char ***);
extern void   __gnat_os_exit			   (int);
extern void   __gnat_set_env_value		   (char *, char *);
extern char  *__gnat_get_libraries_from_registry   (void);
extern int    __gnat_to_canonical_file_list_init   (char *, int);
extern char  *__gnat_to_canonical_file_list_next   (void);
extern void   __gnat_to_canonical_file_list_free   (void);
extern char  *__gnat_to_canonical_dir_spec         (char *, int);
extern char  *__gnat_to_canonical_file_spec        (char *);
extern char  *__gnat_to_host_dir_spec              (char *, int);
extern char  *__gnat_to_host_file_spec             (char *);
extern char  *__gnat_to_canonical_path_spec	   (char *);
extern void   __gnat_adjust_os_resource_limits	   (void);
extern void   convert_addresses			   (void *, int,
						    void *, int *);
extern int    __gnat_copy_attribs		   (char *, char *, int);
extern int    __gnat_feof		  	   (FILE *);
extern int    __gnat_ferror                        (FILE *);
extern int    __gnat_fileno		  	   (FILE *);
extern int    __gnat_is_regular_file_fd  	   (int);
extern FILE  *__gnat_constant_stderr	  	   (void);
extern FILE  *__gnat_constant_stdin	  	   (void);
extern FILE  *__gnat_constant_stdout	  	   (void);
extern char  *__gnat_full_name		  	   (char *, char *);

extern int    __gnat_arg_count			   (void);
extern int    __gnat_len_arg			   (int);
extern void   __gnat_fill_arg			   (char *, int);
extern int    __gnat_env_count			   (void);
extern int    __gnat_len_env			   (int);
extern void   __gnat_fill_env			   (char *, int);

/* Routines for interface to scanf and printf functions for integer values */

extern int    get_int				   (void);
extern void   put_int				   (int);
extern void   put_int_stderr			   (int);
extern int    get_char				   (void);
extern void   put_char				   (int);
extern void   put_char_stderr			   (int);
extern char  *mktemp				   (char *);

extern void   __gnat_set_exit_status		   (int);

extern int    __gnat_expect_fork		   (void);
extern void   __gnat_expect_portable_execvp	   (char *, char *[]);
extern int    __gnat_pipe			   (int *);
extern int    __gnat_expect_poll		   (int *, int, int, int *);
extern void   __gnat_set_binary_mode		   (int);
extern void   __gnat_set_text_mode		   (int);
extern char  *__gnat_ttyname			   (int);
extern int    __gnat_lseek			   (int, long, int);
extern int    __gnat_set_close_on_exec		   (int, int);
extern int    __gnat_dup			   (int);
extern int    __gnat_dup2			   (int, int);

#ifdef __MINGW32__
extern void   __gnat_plist_init                    (void);
#endif

#ifdef IN_RTS
/* Portable definition of strdup, which is not available on all systems.  */
#define xstrdup(S)  strcpy ((char *) malloc (strlen (S) + 1), S)
#endif

/* This function returns the version of GCC being used.  Here it's GCC 3.  */
extern int get_gcc_version		     (void);

/* This function offers a hook for libgnarl to set the
   locking subprograms for libgcc_eh. */
extern void __gnatlib_install_locks	     (void (*) (void),
					      void (*) (void));
