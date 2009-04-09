/* libgcc functions for Blackfin.
   Copyright (C) 2005, 2009 Free Software Foundation, Inc.
   Contributed by Analog Devices.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifdef L_divsi3
.text
.align 2
.global ___divsi3;
.type ___divsi3, STT_FUNC;

___divsi3:
        [--SP]= RETS;
	[--SP] = R7;

	R2 = -R0;
        CC = R0 < 0;
	IF CC R0 = R2;
	R7 = CC;

	R2 = -R1;
        CC = R1 < 0;
	IF CC R1 = R2;
	R2 = CC;
	R7 = R7 ^ R2;

        CALL ___udivsi3;

	CC = R7;
	R1 = -R0;
	IF CC R0 = R1;

	R7 = [SP++];
        RETS = [SP++];
        RTS;
#endif

#ifdef L_modsi3	
.align 2
.global ___modsi3;
.type ___modsi3, STT_FUNC;

___modsi3:
	[--SP] = RETS;
	[--SP] = R0;
	[--SP] = R1;
	CALL ___divsi3;
	R2 = [SP++];
	R1 = [SP++];
	R2 *= R0;
	R0 = R1 - R2;
	RETS = [SP++];
	RTS; 
#endif

#ifdef L_udivsi3
.align 2
.global ___udivsi3;
.type ___udivsi3, STT_FUNC;

___udivsi3:
        P0 = 32;
        LSETUP (0f, 1f) LC0 = P0;
	/* upper half of dividend */
        R3 = 0;
0:
	/* The first time round in the loop we shift in garbage, but since we
	   perform 33 shifts, it doesn't matter.  */
	R0 = ROT R0 BY 1;
	R3 = ROT R3 BY 1;
	R2 = R3 - R1;
        CC = R3 < R1 (IU);
1:
	/* Last instruction of the loop.  */
	IF ! CC R3 = R2;

	/* Shift in the last bit.  */
	R0 = ROT R0 BY 1;
	/* R0 is the result, R3 contains the remainder.  */
	R0 = ~ R0;
        RTS;
#endif

#ifdef L_umodsi3
.align 2
.global ___umodsi3;
.type ___umodsi3, STT_FUNC;

___umodsi3:
	[--SP] = RETS;
	CALL ___udivsi3;
	R0 = R3;
	RETS = [SP++]; 
	RTS;
#endif

#ifdef L_umulsi3_highpart
.align 2
.global ___umulsi3_highpart;
.type ___umulsi3_highpart, STT_FUNC;

___umulsi3_highpart:
	A1 = R1.L * R0.L (FU);
	A1 = A1 >> 16;
	A0 = R1.H * R0.H, A1 += R1.L * R0.H (FU);
	A1 += R0.L * R1.H (FU);
	A1 = A1 >> 16;
	A0 += A1;
	R0 = A0 (FU);
	RTS;
#endif

#ifdef L_smulsi3_highpart
.align 2
.global ___smulsi3_highpart;
.type ___smulsi3_highpart, STT_FUNC;

___smulsi3_highpart:
	A1 = R1.L * R0.L (FU);
	A1 = A1 >> 16;
	A0 = R0.H * R1.H, A1 += R0.H * R1.L (IS,M);
	A1 += R1.H * R0.L (IS,M);
	A1 = A1 >>> 16;
	R0 = (A0 += A1);
	RTS;
#endif
