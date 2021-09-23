/* Adapted from
   https://github.com/stackpath/rxtxcpu/blob/816d86c5d49c4db2ea5649f6b87e96da5af660f1/cpu.c
   which is MIT-licensed.  */

typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)

extern size_t strlen (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__pure__))
  __attribute__ ((__nonnull__ (1)));
extern char *strdup (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__nonnull__ (1)));
extern char *strsep (char **__restrict __stringp,
		     const char *__restrict __delim)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__nonnull__ (1, 2)));
extern long int strtol (const char *__restrict __nptr,
			char **__restrict __endptr, int __base)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__nonnull__ (1)));
extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

#define CPU_LIST_BASE 10

int parse_cpu_list(char *cpu_list) {
  if (strlen(cpu_list) == 0) {
    return 0;
  }

  char *endptr;
  char *tofree, *string, *range;

  tofree = string = strdup(cpu_list); /* { dg-message "allocated here" } */

  while ((range = strsep(&string, ",")) != NULL) {
    int first = strtol(range, &endptr, CPU_LIST_BASE);
    if (!*endptr) {
      continue;
    }
    char *save = endptr;
    endptr++;
    int last = strtol(endptr, &endptr, CPU_LIST_BASE);
    if (save[0] != '-' || *endptr || last < first) {
      return -1; /* { dg-warning "leak of 'tofree'" } */
    }
  }
  free(tofree);
  return 0;
}
