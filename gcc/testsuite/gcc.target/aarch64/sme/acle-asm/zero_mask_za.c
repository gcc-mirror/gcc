/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#include "test_sme_acle.h"

/*
** zero_mask_za_0:
**	zero	{ *}
**	ret
*/
PROTO (zero_mask_za_0, void, ()) { svzero_mask_za (0); }

/*
** zero_mask_za_01:
**	zero	{ za0\.d }
**	ret
*/
PROTO (zero_mask_za_01, void, ()) { svzero_mask_za (0x01); }

/*
** zero_mask_za_80:
**	zero	{ za7\.d }
**	ret
*/
PROTO (zero_mask_za_80, void, ()) { svzero_mask_za (0x80); }

/*
** zero_mask_za_03:
**	zero	{ za0\.d, za1\.d }
**	ret
*/
PROTO (zero_mask_za_03, void, ()) { svzero_mask_za (0x03); }

/*
** zero_mask_za_09:
**	zero	{ za0\.d, za3\.d }
**	ret
*/
PROTO (zero_mask_za_09, void, ()) { svzero_mask_za (0x09); }

/*
** zero_mask_za_0d:
**	zero	{ za0\.d, za2\.d, za3\.d }
**	ret
*/
PROTO (zero_mask_za_0d, void, ()) { svzero_mask_za (0x0d); }

/*
** zero_mask_za_3c:
**	zero	{ za2\.d, za3\.d, za4\.d, za5\.d }
**	ret
*/
PROTO (zero_mask_za_3c, void, ()) { svzero_mask_za (0x3c); }

/*
** zero_mask_za_5a:
**	zero	{ za1\.d, za3\.d, za4\.d, za6\.d }
**	ret
*/
PROTO (zero_mask_za_5a, void, ()) { svzero_mask_za (0x5a); }

/*
** zero_mask_za_11:
**	zero	{ za0\.s }
**	ret
*/
PROTO (zero_mask_za_11, void, ()) { svzero_mask_za (0x11); }

/*
** zero_mask_za_88:
**	zero	{ za3\.s }
**	ret
*/
PROTO (zero_mask_za_88, void, ()) { svzero_mask_za (0x88); }

/*
** zero_mask_za_33:
**	zero	{ za0\.s, za1\.s }
**	ret
*/
PROTO (zero_mask_za_33, void, ()) { svzero_mask_za (0x33); }

/*
** zero_mask_za_cc:
**	zero	{ za2\.s, za3\.s }
**	ret
*/
PROTO (zero_mask_za_cc, void, ()) { svzero_mask_za (0xcc); }

/*
** zero_mask_za_55:
**	zero	{ za0\.h }
**	ret
*/
PROTO (zero_mask_za_55, void, ()) { svzero_mask_za (0x55); }

/*
** zero_mask_za_aa:
**	zero	{ za1\.h }
**	ret
*/
PROTO (zero_mask_za_aa, void, ()) { svzero_mask_za (0xaa); }

/*
** zero_mask_za_ab:
**	zero	{ za0\.d, za1\.d, za3\.d, za5\.d, za7\.d }
**	ret
*/
PROTO (zero_mask_za_ab, void, ()) { svzero_mask_za (0xab); }

/*
** zero_mask_za_d7:
**	zero	{ za0\.d, za1\.d, za2\.d, za4\.d, za6\.d, za7\.d }
**	ret
*/
PROTO (zero_mask_za_d7, void, ()) { svzero_mask_za (0xd7); }

/*
** zero_mask_za_bf:
**	zero	{ za0\.d, za1\.d, za2\.d, za3\.d, za4\.d, za5\.d, za7\.d }
**	ret
*/
PROTO (zero_mask_za_bf, void, ()) { svzero_mask_za (0xbf); }

/*
** zero_mask_za_ff:
**	zero	{ za }
**	ret
*/
PROTO (zero_mask_za_ff, void, ()) { svzero_mask_za (0xff); }
