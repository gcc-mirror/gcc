/* { dg-do compile } */
/* { dg-options "-fpermissive -w" } */

__extension__ typedef __SIZE_TYPE__ size_t;
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;
__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
__extension__ typedef __u_quad_t __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef __u_quad_t __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef __quad_t __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct {
    int __val[2];
} __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned int __id_t;
typedef struct _IO_FILE FILE;
typedef struct _IO_FILE __FILE;
typedef struct {
    int __count;
    union   {
	unsigned int __wch;
	char __wchb[4];
    }
    __value;
} __mbstate_t;
typedef struct {
    __off_t __pos;
    __mbstate_t __state;
} _G_fpos_t;
typedef struct {
    __off64_t __pos;
    __mbstate_t __state;
} _G_fpos64_t;
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef struct _IO_FILE _IO_FILE;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
extern int fputs (__const char *__restrict __s, FILE * __restrict __stream);
extern char *strstr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nonnull__ (1, 2)));
     extern char *rindex (__const char *__s, int __c)   __attribute__ ((__nonnull__ (1)));
extern size_t strlen (__const char *__s) __attribute__ ((__nothrow__))
     __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
     char *mystrlwr (char *string);
     char *m_replace_filename (const char *path, const char *filename);
     typedef struct LINE {
	 char *text;
	 struct LINE *next;
	 int flags;
     } LINE;
typedef struct TOC {
    char *text;
    char *alt;
    struct TOC *next;
    int root;
    int texinfoable;
    int htmlable;
    int otherfile;
} TOC;
static int _empty_count;
extern char *_word_substitution[256];
static void _output_sorted_nested_toc (TOC ** list, unsigned int num_items);
static char *_do_text_substitution (char *input);
static int _toc_scmp (const void *e1, const void *e2)
{
  TOC *t1 = *((TOC **) e1);
  TOC *t2 = *((TOC **) e2);
  return mystricmp (t1->text, t2->text);
}
static void _output_buffered_text (void) { if (_empty_count) ; }
void _post_process_filename (char *filename)
{
  int code_scanning = 0;
  char *new_filename, *p;
  char *line;
  FILE *f1 = 0, *f2 = 0;
  if (!new_filename || strlen (new_filename) < 2)
    new_filename[strlen (filename) - 1] = 'x';
  if (!f1 || !f2)
    while ((line = m_fgets (f1)))
      {
	line = _do_text_substitution (line);
	fputs (line, f2);
      }
  if (remove (filename))
    cancel:
	if (f1)
	  if (f2)
	    if (new_filename)
	      ;
}
static void _close_html_file (FILE * file)
{
  int f;
  for (f = 0; _word_substitution[f]; f++)
    ;
}
static __attribute__((always_inline))
char * _do_text_substitution (char *input)
{
  int start, end, middle, f;
  char *temp, *found, *reader;
  for (f = 0; _word_substitution[f]; f++)
    {
      reader = input;
      while ((found = strstr (reader, _word_substitution[f])))
	{
	  start = found - input;
	  input = temp;
	  reader = temp + start + middle;
	}
    }
  return input;
}
