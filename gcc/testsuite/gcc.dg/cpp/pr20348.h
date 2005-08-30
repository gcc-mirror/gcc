#ifdef MIDDLE
# ifndef PR20348_H_SEEN
#  define PR20348_H_SEEN
# else
#  error pr20348.h included twice after MIDDLE definition
# endif
#else
# error pr20348.h included before MIDDLE definition
#endif
