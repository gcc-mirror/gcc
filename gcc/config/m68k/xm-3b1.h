/* Override part of the obstack macros.  */

#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))
