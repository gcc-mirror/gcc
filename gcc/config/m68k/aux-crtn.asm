/* More startup code for A/UX */

#include "tconfig.h"

#ifdef USE_BIN_AS
	file	"crtn.s"

	init

	unlk %fp
	rts
#else
	.file "crtn.s"

.section .init, "x"
	unlk %fp
	rts

#ifndef USE_COLLECT2
.section .ctors, "d"
	.long 0

.section .dtors, "d"
	.long 0
#endif /* USE_COLLECT2 */
#endif /* USE_BIN_AS */
