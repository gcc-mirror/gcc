/* Some systems provide no sys_siglist, but do offer the same data under
   another name.  */

#define sys_siglist _sys_siglist
#undef SYS_SIGLIST_DECLARED
#define SYS_SIGLIST_DECLARED
