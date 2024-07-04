/* PR tree-optimization/113753 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=gnu23" } */

_BitInt(161) a = 1461501637330902918203684832716283019655932542975wb / 2wb * 2wb;
_BitInt(160) b = 730750818665451459101842416358141509827966271487wb / 2wb * 2wb;
_BitInt(159) c = 365375409332725729550921208179070754913983135743wb / 2wb * 2wb;
_BitInt(129) d = 340282366920938463463374607431768211455wb / 2wb * 2wb;
_BitInt(128) e = 170141183460469231731687303715884105727wb / 2wb * 2wb;
_BitInt(161) f = (-1461501637330902918203684832716283019655932542975wb - 1wb) / 2wb * 2wb;
_BitInt(160) g = (-730750818665451459101842416358141509827966271487wb - 1wb) / 2wb * 2wb;
_BitInt(159) h = (-365375409332725729550921208179070754913983135743wb - 1wb) / 2wb * 2wb;
_BitInt(129) i = (-340282366920938463463374607431768211455wb - 1wb) / 2wb * 2wb;
_BitInt(128) j = (-170141183460469231731687303715884105727wb - 1wb) / 2wb * 2wb;
_BitInt(161) k = 1461501637330902918203684832716283019655932542975wb / 2wb * 3wb;		/* { dg-warning "integer overflow in expression of type '_BitInt\\\(161\\\)' results in" } */
_BitInt(160) l = 730750818665451459101842416358141509827966271487wb / 2wb * 3wb;		/* { dg-warning "integer overflow in expression of type '_BitInt\\\(160\\\)' results in" } */
_BitInt(159) m = 365375409332725729550921208179070754913983135743wb / 2wb * 3wb;		/* { dg-warning "integer overflow in expression of type '_BitInt\\\(159\\\)' results in" } */
_BitInt(129) n = 340282366920938463463374607431768211455wb / 2wb * 3wb;				/* { dg-warning "integer overflow in expression of type '_BitInt\\\(129\\\)' results in" } */
_BitInt(128) o = 170141183460469231731687303715884105727wb / 2wb * 3wb;				/* { dg-warning "integer overflow in expression of type '_BitInt\\\(128\\\)' results in" } */
_BitInt(161) p = (-1461501637330902918203684832716283019655932542975wb - 1wb) / 2wb * 3wb;	/* { dg-warning "integer overflow in expression of type '_BitInt\\\(161\\\)' results in" } */
_BitInt(160) q = (-730750818665451459101842416358141509827966271487wb - 1wb) / 2wb * 3wb;	/* { dg-warning "integer overflow in expression of type '_BitInt\\\(160\\\)' results in" } */
_BitInt(159) r = (-365375409332725729550921208179070754913983135743wb - 1wb) / 2wb * 3wb;	/* { dg-warning "integer overflow in expression of type '_BitInt\\\(159\\\)' results in" } */
_BitInt(129) s = (-340282366920938463463374607431768211455wb - 1wb) / 2wb * 3wb;		/* { dg-warning "integer overflow in expression of type '_BitInt\\\(129\\\)' results in" } */
_BitInt(128) t = (-170141183460469231731687303715884105727wb - 1wb) / 2wb * 3wb;		/* { dg-warning "integer overflow in expression of type '_BitInt\\\(128\\\)' results in" } */
