
/* Include the old config.h as config2.h to simplify the transition
   to autoconf.  */
#include "config2.h"

/* Whether malloc must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_MALLOC

/* Whether realloc must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_REALLOC

/* Whether calloc must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_CALLOC

/* Whether free must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_FREE
@TOP@
