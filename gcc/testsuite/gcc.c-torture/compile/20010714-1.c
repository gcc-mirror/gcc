/* Test that prefix attributes after a comma only apply to a single
   declared object or function.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */

__attribute__((noreturn)) void d0 (void), __attribute__((format(printf, 1, 2))) d1 (const char *, ...), d2 (void);
