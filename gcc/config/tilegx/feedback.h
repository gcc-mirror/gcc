#ifndef _FEEDBACK_H
#define _FEEDBACK_H 1

#ifdef __ASSEMBLER__

/* Stub defines for feedback instrumentation.  */
#define FEEDBACK_ENTER_EXPLICIT(FUNCNAME, SECNAME, SIZE)
#define FEEDBACK_ENTER(FUNCNAME)
#define FEEDBACK_REENTER(FUNCNAME)
#define FEEDBACK_ENTRY(FUNCNAME, SECNAME, SIZE)

#endif /* __ASSEMBLER__ */

#endif /* _FEEDBACK_H */
