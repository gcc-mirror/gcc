/* Test for overly eager multiple include optimization.
   Problem distilled from glibc 2.0.7's time.h, sys/time.h, timebits.h. 
   Problem noted by Tom Tromey <tromey@cygnus.com>.  */
#ifdef need_x
#undef need_x
#ifndef have_x
#define have_x
extern int x;
#endif
#endif

#ifndef t_h
#define t_h
extern int y;
#endif
