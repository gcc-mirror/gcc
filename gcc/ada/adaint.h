/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               A D A I N T                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2009, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#include <sys/stat.h>
#include <stdio.h>

#ifdef _WIN32
#include "mingw32.h"
#endif

#include <dirent.h>

/*  Constants used for the form parameter encoding values  */
#define Encoding_UTF8 0         /* UTF-8 */
#define Encoding_8bits 1        /* Standard 8bits, CP_ACP on Windows. */
#define Encoding_Unspecified 2  /* Based on GNAT_CODE_PAGE env variable. */

/* Large file support. It is unclear what portable mechanism we can use to
   determine at compile time what support the system offers for large files.
   For now we just list the platforms we have manually tested. */

#if defined (__GLIBC__) || defined (sun)  || defined (__sgi)
#define GNAT_FOPEN fopen64
#define GNAT_STAT stat64
#define GNAT_FSTAT fstat64
#define GNAT_LSTAT lstat64
#define GNAT_STRUCT_STAT struct stat64
#else
#define GNAT_FOPEN fopen
#define GNAT_STAT stat
#define GNAT_FSTAT fstat
#define GNAT_LSTAT lstat
#define GNAT_STRUCT_STAT struct stat
#endif

/* Type corresponding to GNAT.OS_Lib.OS_Time */
#if defined (_WIN64)
typedef long long OS_Time;
#else
typedef long OS_Time;
#endif

extern int    __gnat_max_path_len;
extern OS_Time __gnat_current_time		   (void);
extern void   __gnat_current_time_string           (char *);
extern void   __gnat_to_gm_time			   (OS_Time *, int *, int *,
				                    int *, int *,
				                    int *, int *);
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
						    GNAT_STRUCT_STAT *);
extern int    __gnat_unlink                        (char *);
extern int    __gnat_rename                        (char *, char *);
extern int    __gnat_chdir                         (char *);
extern int    __gnat_rmdir                         (char *);

extern FILE  *__gnat_fopen			   (char *, char *, int);
extern FILE  *__gnat_freopen			   (char *, char *, FILE *,
				                    int);
extern int    __gnat_open_read                     (char *, int);
extern int    __gnat_open_rw                       (char *, int);
extern int    __gnat_open_create                   (char *, int);
extern int    __gnat_create_output_file            (char *);
extern int    __gnat_open_append                   (char *, int);
extern long   __gnat_file_length                   (int);
extern long   __gnat_named_file_length             (char *);
extern void   __gnat_tmp_name			   (char *);
extern DIR   *__gnat_opendir                       (char *);
extern char  *__gnat_readdir                       (DIR *, char *, int *);
extern int    __gnat_closedir                      (DIR *);
extern int    __gnat_readdir_is_thread_safe        (void);

extern OS_Time __gnat_file_time_name                (char *);
extern OS_Time __gnat_file_time_fd                  (int);
/* return -1 in case of error */

extern void   __gnat_set_file_time_name		   (char *, time_t);

extern int    __gnat_dup			   (int);
extern int    __gnat_dup2			   (int, int);
extern int    __gnat_file_exists		   (char *);
extern int    __gnat_is_regular_file               (char *);
extern int    __gnat_is_absolute_path              (char *,int);
extern int    __gnat_is_directory		   (char *);
extern int    __gnat_is_writable_file		   (char *);
extern int    __gnat_is_readable_file		   (char *name);
extern int    __gnat_is_executable_file            (char *name);
extern void   __gnat_set_non_writable              (char *name);
extern void   __gnat_set_writable                  (char *name);
extern void   __gnat_set_executable                (char *name);
extern void   __gnat_set_readable                  (char *name);
extern void   __gnat_set_non_readable              (char *name);
extern int    __gnat_is_symbolic_link		   (char *name);
extern int    __gnat_portable_spawn                (char *[]);
extern int    __gnat_portable_no_block_spawn       (char *[]);
extern int    __gnat_portable_wait                 (int *);
extern char  *__gnat_locate_exec                   (char *, char *);
extern char  *__gnat_locate_exec_on_path	   (char *);
extern char  *__gnat_locate_regular_file           (char *, char *);
extern void   __gnat_maybe_glob_args               (int *, char ***);
extern void   __gnat_os_exit			   (int);
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
extern void   convert_addresses			   (const char *, void *, int,
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

extern void   __gnat_os_filename                   (char *, char *, char *,
						    int *, char *, int *);
#if defined (linux)
extern void   *__gnat_lwp_self			   (void);
#endif

#if defined (_WIN32)
/* Interface to delete a handle from internally maintained list of child
   process handles on Windows */
extern void
__gnat_win32_remove_handle (HANDLE h, int pid);
#endif

#ifdef IN_RTS
/* Portable definition of strdup, which is not available on all systems.  */
#define xstrdup(S)  strcpy ((char *) malloc (strlen (S) + 1), S)
#endif

/* This function returns the version of GCC being used.  Here it's GCC 3.  */
extern int    get_gcc_version                      (void);

extern int    __gnat_binder_supports_auto_init     (void);
extern int    __gnat_sals_init_using_constructors  (void);
