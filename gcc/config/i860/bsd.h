#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860, BSD)")

/* BSD UN*X systems use BSD STABS debugging info.  */

#define DBX_DEBUGGING_INFO

#define ASCII_DATA_ASM_OP "\t.byte\t"
#define	ASM_OUTPUT_ASCII(f, p, size)	\
do { register size_t i, limit = (size);	\
  int inside;				\
  inside = FALSE;			\
  for (i = 0; i < limit; i++) {	\
    if (i % 64 == 0) {			\
      if (i != 0) {			\
	if (inside)			\
	  putc('"', (f));		\
	putc('\n', (f));		\
	inside = FALSE;			\
      }					\
      fprintf((f), "%s", ASCII_DATA_ASM_OP);	\
    }					\
    if ((p)[i] < 32 || (p)[i] == '\\' || (p)[i] == '"' || (p)[i] >= 127) {	\
      if (inside) {			\
	putc('"', (f));			\
	inside = FALSE;			\
      }					\
      if (i % 64 != 0)			\
	putc(',', (f));			\
      fprintf((f), "%d", (p)[i]);	\
    } else {				\
      if (!inside) {			\
	if (i % 64 != 0)		\
	  putc(',', (f));		\
	putc('"', (f));			\
	inside = TRUE;			\
      }					\
      putc((p)[i], (f));		\
    }					\
  }					\
  if (inside)				\
    putc('"', (f));			\
  putc('\n', (f));			\
} while (0)
