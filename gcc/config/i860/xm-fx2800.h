/* Alliant FX/2800 running Concentrix 2.x. */

/* vfprintf is not present prior to Concentrix 2.2. Unfortunately, there
   does not seem to be a cpp symbol that identifies OS revision. Undefine
   the following if running 2.1 or older.  */

#define HAVE_VPRINTF
