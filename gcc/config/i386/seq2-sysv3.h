/* Sequent DYNIX/ptx 2.x (SVr3) */

/* Use atexit for static destructors, instead of defining
   our own exit function.  */
#undef NEED_ATEXIT
