// Build don't link:
// prms-id: 12475
// excess errors test - XFAIL alpha*-*-* mips64*-*-*

enum huh { start =-2147483648, next };		// WARNING - , XFAIL sparc64-*-* alpha*-*-*
