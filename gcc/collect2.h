#ifndef __COLLECT2_H__
#define __COLLECT2_H__

extern void do_tlink PARAMS ((char **, char **));

extern void collect_execute PARAMS ((char *, char **, char *));

extern void collect_exit PARAMS ((int)) ATTRIBUTE_NORETURN;

extern int collect_wait PARAMS ((char *));

extern void dump_file PARAMS ((char *));

extern int file_exists PARAMS ((char *));

#endif /* ! __COLLECT2_H__ */
