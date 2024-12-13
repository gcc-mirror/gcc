/* Test characters not permitted in UCNs in C17: -std=c23 -Wc11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

#if U'\u0000' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu0 = U"\u0000"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000000' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU0 = U"\U00000000"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0001' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu1 = U"\u0001"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000001' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU1 = U"\U00000001"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0002' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu2 = U"\u0002"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000002' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU2 = U"\U00000002"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0003' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu3 = U"\u0003"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000003' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU3 = U"\U00000003"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0004' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu4 = U"\u0004"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000004' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU4 = U"\U00000004"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0005' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu5 = U"\u0005"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000005' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU5 = U"\U00000005"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0006' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu6 = U"\u0006"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000006' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU6 = U"\U00000006"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0007' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu7 = U"\u0007"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000007' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU7 = U"\U00000007"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0008' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu8 = U"\u0008"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000008' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU8 = U"\U00000008"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0009' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu9 = U"\u0009"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000009' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU9 = U"\U00000009"; /* { dg-warning "is not a valid universal character" } */
#if U'\u000a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu10 = U"\u000a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000000a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU10 = U"\U0000000a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u000b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu11 = U"\u000b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000000b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU11 = U"\U0000000b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u000c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu12 = U"\u000c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000000c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU12 = U"\U0000000c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u000d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu13 = U"\u000d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000000d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU13 = U"\U0000000d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u000e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu14 = U"\u000e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000000e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU14 = U"\U0000000e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u000f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu15 = U"\u000f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000000f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU15 = U"\U0000000f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0010' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu16 = U"\u0010"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000010' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU16 = U"\U00000010"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0011' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu17 = U"\u0011"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000011' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU17 = U"\U00000011"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0012' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu18 = U"\u0012"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000012' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU18 = U"\U00000012"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0013' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu19 = U"\u0013"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000013' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU19 = U"\U00000013"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0014' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu20 = U"\u0014"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000014' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU20 = U"\U00000014"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0015' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu21 = U"\u0015"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000015' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU21 = U"\U00000015"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0016' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu22 = U"\u0016"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000016' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU22 = U"\U00000016"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0017' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu23 = U"\u0017"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000017' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU23 = U"\U00000017"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0018' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu24 = U"\u0018"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000018' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU24 = U"\U00000018"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0019' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu25 = U"\u0019"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000019' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU25 = U"\U00000019"; /* { dg-warning "is not a valid universal character" } */
#if U'\u001a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu26 = U"\u001a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000001a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU26 = U"\U0000001a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u001b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu27 = U"\u001b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000001b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU27 = U"\U0000001b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u001c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu28 = U"\u001c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000001c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU28 = U"\U0000001c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u001d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu29 = U"\u001d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000001d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU29 = U"\U0000001d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u001e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu30 = U"\u001e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000001e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU30 = U"\U0000001e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u001f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu31 = U"\u001f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000001f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU31 = U"\U0000001f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0020' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu32 = U"\u0020"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000020' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU32 = U"\U00000020"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0021' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu33 = U"\u0021"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000021' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU33 = U"\U00000021"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0022' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu34 = U"\u0022"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000022' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU34 = U"\U00000022"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0023' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu35 = U"\u0023"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000023' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU35 = U"\U00000023"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0024'
#endif
void *tu36 = U"\u0024";
#if U'\U00000024'
#endif
void *tU36 = U"\U00000024";
#if U'\u0025' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu37 = U"\u0025"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000025' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU37 = U"\U00000025"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0026' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu38 = U"\u0026"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000026' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU38 = U"\U00000026"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0027' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu39 = U"\u0027"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000027' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU39 = U"\U00000027"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0028' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu40 = U"\u0028"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000028' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU40 = U"\U00000028"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0029' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu41 = U"\u0029"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000029' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU41 = U"\U00000029"; /* { dg-warning "is not a valid universal character" } */
#if U'\u002a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu42 = U"\u002a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000002a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU42 = U"\U0000002a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u002b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu43 = U"\u002b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000002b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU43 = U"\U0000002b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u002c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu44 = U"\u002c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000002c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU44 = U"\U0000002c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u002d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu45 = U"\u002d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000002d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU45 = U"\U0000002d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u002e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu46 = U"\u002e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000002e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU46 = U"\U0000002e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u002f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu47 = U"\u002f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000002f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU47 = U"\U0000002f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0030' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu48 = U"\u0030"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000030' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU48 = U"\U00000030"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0031' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu49 = U"\u0031"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000031' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU49 = U"\U00000031"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0032' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu50 = U"\u0032"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000032' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU50 = U"\U00000032"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0033' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu51 = U"\u0033"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000033' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU51 = U"\U00000033"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0034' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu52 = U"\u0034"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000034' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU52 = U"\U00000034"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0035' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu53 = U"\u0035"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000035' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU53 = U"\U00000035"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0036' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu54 = U"\u0036"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000036' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU54 = U"\U00000036"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0037' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu55 = U"\u0037"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000037' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU55 = U"\U00000037"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0038' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu56 = U"\u0038"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000038' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU56 = U"\U00000038"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0039' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu57 = U"\u0039"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000039' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU57 = U"\U00000039"; /* { dg-warning "is not a valid universal character" } */
#if U'\u003a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu58 = U"\u003a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000003a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU58 = U"\U0000003a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u003b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu59 = U"\u003b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000003b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU59 = U"\U0000003b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u003c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu60 = U"\u003c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000003c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU60 = U"\U0000003c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u003d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu61 = U"\u003d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000003d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU61 = U"\U0000003d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u003e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu62 = U"\u003e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000003e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU62 = U"\U0000003e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u003f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu63 = U"\u003f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000003f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU63 = U"\U0000003f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0040'
#endif
void *tu64 = U"\u0040";
#if U'\U00000040'
#endif
void *tU64 = U"\U00000040";
#if U'\u0041' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu65 = U"\u0041"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000041' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU65 = U"\U00000041"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0042' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu66 = U"\u0042"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000042' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU66 = U"\U00000042"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0043' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu67 = U"\u0043"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000043' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU67 = U"\U00000043"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0044' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu68 = U"\u0044"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000044' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU68 = U"\U00000044"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0045' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu69 = U"\u0045"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000045' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU69 = U"\U00000045"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0046' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu70 = U"\u0046"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000046' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU70 = U"\U00000046"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0047' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu71 = U"\u0047"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000047' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU71 = U"\U00000047"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0048' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu72 = U"\u0048"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000048' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU72 = U"\U00000048"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0049' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu73 = U"\u0049"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000049' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU73 = U"\U00000049"; /* { dg-warning "is not a valid universal character" } */
#if U'\u004a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu74 = U"\u004a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000004a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU74 = U"\U0000004a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u004b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu75 = U"\u004b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000004b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU75 = U"\U0000004b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u004c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu76 = U"\u004c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000004c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU76 = U"\U0000004c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u004d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu77 = U"\u004d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000004d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU77 = U"\U0000004d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u004e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu78 = U"\u004e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000004e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU78 = U"\U0000004e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u004f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu79 = U"\u004f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000004f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU79 = U"\U0000004f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0050' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu80 = U"\u0050"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000050' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU80 = U"\U00000050"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0051' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu81 = U"\u0051"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000051' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU81 = U"\U00000051"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0052' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu82 = U"\u0052"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000052' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU82 = U"\U00000052"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0053' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu83 = U"\u0053"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000053' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU83 = U"\U00000053"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0054' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu84 = U"\u0054"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000054' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU84 = U"\U00000054"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0055' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu85 = U"\u0055"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000055' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU85 = U"\U00000055"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0056' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu86 = U"\u0056"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000056' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU86 = U"\U00000056"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0057' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu87 = U"\u0057"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000057' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU87 = U"\U00000057"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0058' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu88 = U"\u0058"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000058' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU88 = U"\U00000058"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0059' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu89 = U"\u0059"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000059' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU89 = U"\U00000059"; /* { dg-warning "is not a valid universal character" } */
#if U'\u005a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu90 = U"\u005a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000005a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU90 = U"\U0000005a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u005b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu91 = U"\u005b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000005b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU91 = U"\U0000005b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u005c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu92 = U"\u005c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000005c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU92 = U"\U0000005c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u005d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu93 = U"\u005d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000005d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU93 = U"\U0000005d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u005e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu94 = U"\u005e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000005e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU94 = U"\U0000005e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u005f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu95 = U"\u005f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000005f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU95 = U"\U0000005f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0060'
#endif
void *tu96 = U"\u0060";
#if U'\U00000060'
#endif
void *tU96 = U"\U00000060";
#if U'\u0061' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu97 = U"\u0061"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000061' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU97 = U"\U00000061"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0062' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu98 = U"\u0062"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000062' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU98 = U"\U00000062"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0063' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu99 = U"\u0063"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000063' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU99 = U"\U00000063"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0064' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu100 = U"\u0064"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000064' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU100 = U"\U00000064"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0065' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu101 = U"\u0065"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000065' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU101 = U"\U00000065"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0066' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu102 = U"\u0066"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000066' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU102 = U"\U00000066"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0067' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu103 = U"\u0067"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000067' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU103 = U"\U00000067"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0068' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu104 = U"\u0068"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000068' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU104 = U"\U00000068"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0069' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu105 = U"\u0069"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000069' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU105 = U"\U00000069"; /* { dg-warning "is not a valid universal character" } */
#if U'\u006a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu106 = U"\u006a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000006a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU106 = U"\U0000006a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u006b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu107 = U"\u006b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000006b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU107 = U"\U0000006b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u006c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu108 = U"\u006c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000006c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU108 = U"\U0000006c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u006d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu109 = U"\u006d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000006d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU109 = U"\U0000006d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u006e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu110 = U"\u006e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000006e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU110 = U"\U0000006e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u006f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu111 = U"\u006f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000006f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU111 = U"\U0000006f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0070' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu112 = U"\u0070"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000070' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU112 = U"\U00000070"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0071' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu113 = U"\u0071"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000071' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU113 = U"\U00000071"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0072' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu114 = U"\u0072"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000072' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU114 = U"\U00000072"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0073' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu115 = U"\u0073"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000073' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU115 = U"\U00000073"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0074' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu116 = U"\u0074"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000074' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU116 = U"\U00000074"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0075' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu117 = U"\u0075"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000075' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU117 = U"\U00000075"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0076' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu118 = U"\u0076"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000076' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU118 = U"\U00000076"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0077' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu119 = U"\u0077"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000077' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU119 = U"\U00000077"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0078' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu120 = U"\u0078"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000078' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU120 = U"\U00000078"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0079' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu121 = U"\u0079"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000079' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU121 = U"\U00000079"; /* { dg-warning "is not a valid universal character" } */
#if U'\u007a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu122 = U"\u007a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000007a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU122 = U"\U0000007a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u007b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu123 = U"\u007b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000007b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU123 = U"\U0000007b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u007c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu124 = U"\u007c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000007c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU124 = U"\U0000007c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u007d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu125 = U"\u007d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000007d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU125 = U"\U0000007d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u007e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu126 = U"\u007e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000007e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU126 = U"\U0000007e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u007f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu127 = U"\u007f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000007f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU127 = U"\U0000007f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0080' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu128 = U"\u0080"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000080' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU128 = U"\U00000080"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0081' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu129 = U"\u0081"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000081' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU129 = U"\U00000081"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0082' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu130 = U"\u0082"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000082' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU130 = U"\U00000082"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0083' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu131 = U"\u0083"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000083' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU131 = U"\U00000083"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0084' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu132 = U"\u0084"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000084' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU132 = U"\U00000084"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0085' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu133 = U"\u0085"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000085' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU133 = U"\U00000085"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0086' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu134 = U"\u0086"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000086' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU134 = U"\U00000086"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0087' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu135 = U"\u0087"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000087' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU135 = U"\U00000087"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0088' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu136 = U"\u0088"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000088' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU136 = U"\U00000088"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0089' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu137 = U"\u0089"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000089' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU137 = U"\U00000089"; /* { dg-warning "is not a valid universal character" } */
#if U'\u008a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu138 = U"\u008a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000008a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU138 = U"\U0000008a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u008b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu139 = U"\u008b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000008b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU139 = U"\U0000008b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u008c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu140 = U"\u008c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000008c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU140 = U"\U0000008c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u008d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu141 = U"\u008d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000008d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU141 = U"\U0000008d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u008e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu142 = U"\u008e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000008e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU142 = U"\U0000008e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u008f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu143 = U"\u008f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000008f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU143 = U"\U0000008f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0090' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu144 = U"\u0090"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000090' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU144 = U"\U00000090"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0091' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu145 = U"\u0091"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000091' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU145 = U"\U00000091"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0092' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu146 = U"\u0092"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000092' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU146 = U"\U00000092"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0093' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu147 = U"\u0093"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000093' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU147 = U"\U00000093"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0094' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu148 = U"\u0094"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000094' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU148 = U"\U00000094"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0095' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu149 = U"\u0095"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000095' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU149 = U"\U00000095"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0096' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu150 = U"\u0096"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000096' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU150 = U"\U00000096"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0097' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu151 = U"\u0097"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000097' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU151 = U"\U00000097"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0098' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu152 = U"\u0098"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000098' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU152 = U"\U00000098"; /* { dg-warning "is not a valid universal character" } */
#if U'\u0099' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu153 = U"\u0099"; /* { dg-warning "is not a valid universal character" } */
#if U'\U00000099' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU153 = U"\U00000099"; /* { dg-warning "is not a valid universal character" } */
#if U'\u009a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu154 = U"\u009a"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000009a' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU154 = U"\U0000009a"; /* { dg-warning "is not a valid universal character" } */
#if U'\u009b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu155 = U"\u009b"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000009b' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU155 = U"\U0000009b"; /* { dg-warning "is not a valid universal character" } */
#if U'\u009c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu156 = U"\u009c"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000009c' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU156 = U"\U0000009c"; /* { dg-warning "is not a valid universal character" } */
#if U'\u009d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu157 = U"\u009d"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000009d' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU157 = U"\U0000009d"; /* { dg-warning "is not a valid universal character" } */
#if U'\u009e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu158 = U"\u009e"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000009e' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU158 = U"\U0000009e"; /* { dg-warning "is not a valid universal character" } */
#if U'\u009f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tu159 = U"\u009f"; /* { dg-warning "is not a valid universal character" } */
#if U'\U0000009f' /* { dg-warning "is not a valid universal character" } */
#endif
void *tU159 = U"\U0000009f"; /* { dg-warning "is not a valid universal character" } */
#if U'\u00a0'
#endif
void *tu160 = U"\u00a0";
#if U'\U000000a0'
#endif
void *tU160 = U"\U000000a0";

#if U'\ud800' /* { dg-error "is not a valid universal character" } */
#endif
void *tud800 = U"\ud800"; /* { dg-error "is not a valid universal character" } */
#if U'\U0000d800' /* { dg-error "is not a valid universal character" } */
#endif
void *tUd800 = U"\U0000d800"; /* { dg-error "is not a valid universal character" } */

#if U'\udfff' /* { dg-error "is not a valid universal character" } */
#endif
void *tudfff = U"\udfff"; /* { dg-error "is not a valid universal character" } */
#if U'\U0000dfff' /* { dg-error "is not a valid universal character" } */
#endif
void *tUdfff = U"\U0000dfff"; /* { dg-error "is not a valid universal character" } */

#if U'\U0010ffff'
#endif
void *tU10ffff = U"\U0010ffff";

#if U'\U00110000' /* { dg-warning "is outside the UCS codespace" } */
#endif
void *tU110000 = U"\U00110000"; /* { dg-warning "is outside the UCS codespace" } */

#if U'\Uffffffff' /* { dg-error "is not a valid universal character" } */
#endif
void *tUffffffff = U"\Uffffffff"; /* { dg-error "is not a valid universal character" } */
