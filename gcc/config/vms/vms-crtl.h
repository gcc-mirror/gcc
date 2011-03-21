/* Definitions of target machine GNU compiler. 32bit VMS version.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp (rupp@gnat.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* 
   Correlation array of standard CRTL names with DECCRTL
   function names. Currently contains only a partial list,
   e.g. those functions use in GNAT and GCC

   Note: Please keep in alphabetical order.
*/

#define CRTL_NAMES                          \
{                                           \
{"_calloc32",    "decc$calloc",       0},   \
{"_malloc32",    "decc$malloc",       0},   \
{"_realloc32",   "decc$realloc",      0},   \
{"_strdup32",    "decc$strdup",       0},   \
{"abs",          "decc$abs",          0},   \
{"abort",        "decc$abort",        0},   \
{"access",       "decc$access",       0},   \
{"accept",       "decc$accept",       0},   \
{"acos",         "decc$tacos",        0},   \
{"alarm",        "decc$alarm",        0},   \
{"asin",         "decc$tasin",        0},   \
{"atan",         "decc$tatan",        0},   \
{"atan2",        "decc$tatan2",       0},   \
{"atexit",       "decc$atexit",       0},   \
{"atoi",         "decc$atoi",         0},   \
{"atoll",        "decc$atoll",        0},   \
{"atoq",         "decc$atoq",         0},   \
{"basename",     "decc$basename",     0},   \
{"bcmp",         "decc$bcmp",         0},   \
{"bcopy",        "decc$bcopy",        0},   \
{"bsearch",      "decc$bsearch",      0},   \
{"bzero",        "decc$bzero",        0},   \
{"calloc",       "decc$calloc",       0},   \
{"ceil",         "decc$tceil",        0},   \
{"chdir",        "decc$chdir",        0},   \
{"chown",        "decc$chown",        0},   \
{"clearerr",     "decc$clearerr",     0},   \
{"clock",        "decc$clock",        0},   \
{"close",        "decc$close",        0},   \
{"cos",          "decc$tcos",         0},   \
{"connect",      "decc$connect",      0},   \
{"ctime",        "decc$ctime",        0},   \
{"dup",          "decc$dup",          0},   \
{"dup2",         "decc$dup2",         0},   \
{"exit",         "decc$exit",         0},   \
{"exp",          "decc$texp",         0},   \
{"fabs",         "decc$tfabs",        0},   \
{"fclose",       "decc$fclose",       0},   \
{"fdopen",       "decc$fdopen",       0},   \
{"fgetc",        "decc$fgetc",        0},   \
{"fgets",        "decc$fgets",        0},   \
{"fflush",       "decc$fflush",       0},   \
{"ffs",          "decc$ffs",          0},   \
{"floor",        "decc$tfloor",       0},   \
{"fopen",        "decc$fopen",        0},   \
{"fputc",        "decc$fputc",        0},   \
{"fputs",        "decc$fputs",        0},   \
{"free",         "decc$free",         0},   \
{"fread",        "decc$fread",        0},   \
{"freopen",      "decc$freopen",      0},   \
{"fseek",        "decc$fseek",        0},   \
{"ftell",        "decc$ftell",        0},   \
{"fwrite",       "decc$fwrite",       0},   \
{"getcwd",       "decc$getcwd",       0},   \
{"getegid",      "decc$getegid",      0},   \
{"getenv",       "decc$getenv",       0},   \
{"geteuid",      "decc$geteuid",      0},   \
{"getgid",       "decc$getgid",       0},   \
{"gethostbyaddr","decc$gethostbyaddr",0},   \
{"gethostbyname","decc$gethostbyname",0},   \
{"getpagesize",  "decc$getpagesize",  0},   \
{"getpid",       "decc$getpid",       0},   \
{"getservbyname","decc$getservbyname",0},   \
{"getservbyport","decc$getservbyport",0},   \
{"gettimeofday", "decc$gettimeofday", 0},   \
{"getuid",       "decc$getuid",       0},   \
{"htons",        "decc$htons",        0},   \
{"iconv",        "decc$iconv",        0},   \
{"index",        "decc$index",        0},   \
{"isatty",       "decc$isatty",       0},   \
{"isdigit",      "decc$isdigit",      0},   \
{"kill",         "decc$kill",         0},   \
{"log",          "decc$tlog",         0},   \
{"log10",        "decc$tlog10",       0},   \
{"lseek",        "decc$lseek",        0},   \
{"ioctl",        "decc$ioctl",        0},   \
{"malloc",       "decc$malloc",       0},   \
{"mbstowcs",     "decc$mbstowcs",     0},   \
{"memchr",       "decc$memchr",       0},   \
{"memcmp",       "decc$memcmp",       0},   \
{"memcpy",       "decc$memcpy",       0},   \
{"memmove",      "decc$memmove",      0},   \
{"memset",       "decc$memset",       0},   \
{"mkstemp",      "decc$mkstemp",      0},   \
{"mktemp",       "decc$mktemp",       0},   \
{"mmap",         "decc$mmap",         0},   \
{"munmap",       "decc$munmap",       0},   \
{"nl_langinfo",  "decc$nl_langinfo",  0},   \
{"open",         "decc$open",         0},   \
{"pclose",       "decc$pclose",       0},   \
{"popen",        "decc$popen",        0},   \
{"pow",          "decc$tpow",         0},   \
{"printf",       "decc$txprintf",     0},   \
{"putenv",       "decc$putenv",       0},   \
{"puts",         "decc$puts",         0},   \
{"random",       "decc$random",       0},   \
{"read",         "decc$read",         0},   \
{"realloc",      "decc$realloc",      0},   \
{"recv",         "decc$recv",         0},   \
{"recvfrom",     "decc$recvfrom",     0},   \
{"recvmsg",      "decc$__bsd44___recvmsg32", 0}, \
{"rename",       "decc$rename",       0},   \
{"rewind",       "decc$rewind",       0},   \
{"rindex",       "decc$rindex",       0},   \
{"rmdir",        "decc$rmdir",        0},   \
{"send",         "decc$send",         0},   \
{"sendmsg",      "decc$__bsd44___sendmsg32", 0}, \
{"sendto",       "decc$sendto",       0},   \
{"setenv",       "decc$setenv",       0},   \
{"setlocale",    "decc$setlocale",    0},   \
{"setvbuf",      "decc$setvbuf",      0},   \
{"signal",       "decc$signal",       0},   \
{"sigsetmask",   "decc$sigsetmask",   0},   \
{"sin",          "decc$tsin",         0},   \
{"snprintf",     "decc$txsnprintf",   0},   \
{"socket",       "decc$socket",       0},   \
{"sqrt",         "decc$tsqrt",        0},   \
{"strcasecmp",   "decc$strcasecmp",   0},   \
{"strchr",       "decc$strchr",       0},   \
{"strcpy",       "decc$strcpy",       0},   \
{"strdup",       "decc$strdup",       0},   \
{"strerror",     "decc$strerror",     0},   \
{"strlen",       "decc$strlen",       0},   \
{"strncasecmp",  "decc$strncasecmp",  0},   \
{"strncmp",      "decc$strncmp",      0},   \
{"strncpy",      "decc$strncpy",      0},   \
{"strrchr",      "decc$strrchr",      0},   \
{"strstr",       "decc$strstr",       0},   \
{"strtod",       "decc$tstrtod",      0},   \
{"strtol",       "decc$strtoll",      0},   \
{"strtoul",      "decc$strtoull",     0},   \
{"sysconf",      "decc$sysconf",      0},   \
{"system",       "decc$system",       0},   \
{"tan",          "decc$ttan",         0},   \
{"time",         "decc$time",         0},   \
{"times",        "decc$times",        0},   \
{"tmpfile",      "decc$tmpfile",      0},   \
{"tmpnam",       "decc$tmpnam",       0},   \
{"ungetc",       "decc$ungetc",       0},   \
{"unlink",       "decc$unlink",       0},   \
{"umask",        "decc$umask",        0},   \
{"utime",        "decc$utime",        0},   \
{"wait",         "decc$wait",         0},   \
{"waitpid",      "decc$waitpid",      0},   \
{"wcswidth",     "decc$wcswidth",     0},   \
{"write",        "decc$write",        0},   \
{"vfprintf",     "decc$txvfprintf",   0},   \
{"vprintf",      "decc$txvprintf",    0},   \
{"vsprintf",     "decc$txvsprintf",   0},   \
{"vsnprintf",    "decc$txvsnprintf",  0},   \
{NULL, NULL, 0}                             \
}

/* Initialize of libfuncs that are 32/64 bit memory specific.  */

#define MEM_LIBFUNCS_INIT                              \
do {                                                   \
  memcpy_libfunc = init_one_libfunc ("decc$memcpy");   \
  memmove_libfunc = init_one_libfunc ("decc$memmove"); \
  memset_libfunc = init_one_libfunc ("decc$memset");   \
} while (0)
