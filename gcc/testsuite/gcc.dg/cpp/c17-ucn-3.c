/* Test characters not permitted in UCNs in C17: no warnings without
   -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c17" } */

#if U'\u0000'
#endif
void *tu0 = U"\u0000";
#if U'\U00000000'
#endif
void *tU0 = U"\U00000000";
#if U'\u0001'
#endif
void *tu1 = U"\u0001";
#if U'\U00000001'
#endif
void *tU1 = U"\U00000001";
#if U'\u0002'
#endif
void *tu2 = U"\u0002";
#if U'\U00000002'
#endif
void *tU2 = U"\U00000002";
#if U'\u0003'
#endif
void *tu3 = U"\u0003";
#if U'\U00000003'
#endif
void *tU3 = U"\U00000003";
#if U'\u0004'
#endif
void *tu4 = U"\u0004";
#if U'\U00000004'
#endif
void *tU4 = U"\U00000004";
#if U'\u0005'
#endif
void *tu5 = U"\u0005";
#if U'\U00000005'
#endif
void *tU5 = U"\U00000005";
#if U'\u0006'
#endif
void *tu6 = U"\u0006";
#if U'\U00000006'
#endif
void *tU6 = U"\U00000006";
#if U'\u0007'
#endif
void *tu7 = U"\u0007";
#if U'\U00000007'
#endif
void *tU7 = U"\U00000007";
#if U'\u0008'
#endif
void *tu8 = U"\u0008";
#if U'\U00000008'
#endif
void *tU8 = U"\U00000008";
#if U'\u0009'
#endif
void *tu9 = U"\u0009";
#if U'\U00000009'
#endif
void *tU9 = U"\U00000009";
#if U'\u000a'
#endif
void *tu10 = U"\u000a";
#if U'\U0000000a'
#endif
void *tU10 = U"\U0000000a";
#if U'\u000b'
#endif
void *tu11 = U"\u000b";
#if U'\U0000000b'
#endif
void *tU11 = U"\U0000000b";
#if U'\u000c'
#endif
void *tu12 = U"\u000c";
#if U'\U0000000c'
#endif
void *tU12 = U"\U0000000c";
#if U'\u000d'
#endif
void *tu13 = U"\u000d";
#if U'\U0000000d'
#endif
void *tU13 = U"\U0000000d";
#if U'\u000e'
#endif
void *tu14 = U"\u000e";
#if U'\U0000000e'
#endif
void *tU14 = U"\U0000000e";
#if U'\u000f'
#endif
void *tu15 = U"\u000f";
#if U'\U0000000f'
#endif
void *tU15 = U"\U0000000f";
#if U'\u0010'
#endif
void *tu16 = U"\u0010";
#if U'\U00000010'
#endif
void *tU16 = U"\U00000010";
#if U'\u0011'
#endif
void *tu17 = U"\u0011";
#if U'\U00000011'
#endif
void *tU17 = U"\U00000011";
#if U'\u0012'
#endif
void *tu18 = U"\u0012";
#if U'\U00000012'
#endif
void *tU18 = U"\U00000012";
#if U'\u0013'
#endif
void *tu19 = U"\u0013";
#if U'\U00000013'
#endif
void *tU19 = U"\U00000013";
#if U'\u0014'
#endif
void *tu20 = U"\u0014";
#if U'\U00000014'
#endif
void *tU20 = U"\U00000014";
#if U'\u0015'
#endif
void *tu21 = U"\u0015";
#if U'\U00000015'
#endif
void *tU21 = U"\U00000015";
#if U'\u0016'
#endif
void *tu22 = U"\u0016";
#if U'\U00000016'
#endif
void *tU22 = U"\U00000016";
#if U'\u0017'
#endif
void *tu23 = U"\u0017";
#if U'\U00000017'
#endif
void *tU23 = U"\U00000017";
#if U'\u0018'
#endif
void *tu24 = U"\u0018";
#if U'\U00000018'
#endif
void *tU24 = U"\U00000018";
#if U'\u0019'
#endif
void *tu25 = U"\u0019";
#if U'\U00000019'
#endif
void *tU25 = U"\U00000019";
#if U'\u001a'
#endif
void *tu26 = U"\u001a";
#if U'\U0000001a'
#endif
void *tU26 = U"\U0000001a";
#if U'\u001b'
#endif
void *tu27 = U"\u001b";
#if U'\U0000001b'
#endif
void *tU27 = U"\U0000001b";
#if U'\u001c'
#endif
void *tu28 = U"\u001c";
#if U'\U0000001c'
#endif
void *tU28 = U"\U0000001c";
#if U'\u001d'
#endif
void *tu29 = U"\u001d";
#if U'\U0000001d'
#endif
void *tU29 = U"\U0000001d";
#if U'\u001e'
#endif
void *tu30 = U"\u001e";
#if U'\U0000001e'
#endif
void *tU30 = U"\U0000001e";
#if U'\u001f'
#endif
void *tu31 = U"\u001f";
#if U'\U0000001f'
#endif
void *tU31 = U"\U0000001f";
#if U'\u0020'
#endif
void *tu32 = U"\u0020";
#if U'\U00000020'
#endif
void *tU32 = U"\U00000020";
#if U'\u0021'
#endif
void *tu33 = U"\u0021";
#if U'\U00000021'
#endif
void *tU33 = U"\U00000021";
#if U'\u0022'
#endif
void *tu34 = U"\u0022";
#if U'\U00000022'
#endif
void *tU34 = U"\U00000022";
#if U'\u0023'
#endif
void *tu35 = U"\u0023";
#if U'\U00000023'
#endif
void *tU35 = U"\U00000023";
#if U'\u0024'
#endif
void *tu36 = U"\u0024";
#if U'\U00000024'
#endif
void *tU36 = U"\U00000024";
#if U'\u0025'
#endif
void *tu37 = U"\u0025";
#if U'\U00000025'
#endif
void *tU37 = U"\U00000025";
#if U'\u0026'
#endif
void *tu38 = U"\u0026";
#if U'\U00000026'
#endif
void *tU38 = U"\U00000026";
#if U'\u0027'
#endif
void *tu39 = U"\u0027";
#if U'\U00000027'
#endif
void *tU39 = U"\U00000027";
#if U'\u0028'
#endif
void *tu40 = U"\u0028";
#if U'\U00000028'
#endif
void *tU40 = U"\U00000028";
#if U'\u0029'
#endif
void *tu41 = U"\u0029";
#if U'\U00000029'
#endif
void *tU41 = U"\U00000029";
#if U'\u002a'
#endif
void *tu42 = U"\u002a";
#if U'\U0000002a'
#endif
void *tU42 = U"\U0000002a";
#if U'\u002b'
#endif
void *tu43 = U"\u002b";
#if U'\U0000002b'
#endif
void *tU43 = U"\U0000002b";
#if U'\u002c'
#endif
void *tu44 = U"\u002c";
#if U'\U0000002c'
#endif
void *tU44 = U"\U0000002c";
#if U'\u002d'
#endif
void *tu45 = U"\u002d";
#if U'\U0000002d'
#endif
void *tU45 = U"\U0000002d";
#if U'\u002e'
#endif
void *tu46 = U"\u002e";
#if U'\U0000002e'
#endif
void *tU46 = U"\U0000002e";
#if U'\u002f'
#endif
void *tu47 = U"\u002f";
#if U'\U0000002f'
#endif
void *tU47 = U"\U0000002f";
#if U'\u0030'
#endif
void *tu48 = U"\u0030";
#if U'\U00000030'
#endif
void *tU48 = U"\U00000030";
#if U'\u0031'
#endif
void *tu49 = U"\u0031";
#if U'\U00000031'
#endif
void *tU49 = U"\U00000031";
#if U'\u0032'
#endif
void *tu50 = U"\u0032";
#if U'\U00000032'
#endif
void *tU50 = U"\U00000032";
#if U'\u0033'
#endif
void *tu51 = U"\u0033";
#if U'\U00000033'
#endif
void *tU51 = U"\U00000033";
#if U'\u0034'
#endif
void *tu52 = U"\u0034";
#if U'\U00000034'
#endif
void *tU52 = U"\U00000034";
#if U'\u0035'
#endif
void *tu53 = U"\u0035";
#if U'\U00000035'
#endif
void *tU53 = U"\U00000035";
#if U'\u0036'
#endif
void *tu54 = U"\u0036";
#if U'\U00000036'
#endif
void *tU54 = U"\U00000036";
#if U'\u0037'
#endif
void *tu55 = U"\u0037";
#if U'\U00000037'
#endif
void *tU55 = U"\U00000037";
#if U'\u0038'
#endif
void *tu56 = U"\u0038";
#if U'\U00000038'
#endif
void *tU56 = U"\U00000038";
#if U'\u0039'
#endif
void *tu57 = U"\u0039";
#if U'\U00000039'
#endif
void *tU57 = U"\U00000039";
#if U'\u003a'
#endif
void *tu58 = U"\u003a";
#if U'\U0000003a'
#endif
void *tU58 = U"\U0000003a";
#if U'\u003b'
#endif
void *tu59 = U"\u003b";
#if U'\U0000003b'
#endif
void *tU59 = U"\U0000003b";
#if U'\u003c'
#endif
void *tu60 = U"\u003c";
#if U'\U0000003c'
#endif
void *tU60 = U"\U0000003c";
#if U'\u003d'
#endif
void *tu61 = U"\u003d";
#if U'\U0000003d'
#endif
void *tU61 = U"\U0000003d";
#if U'\u003e'
#endif
void *tu62 = U"\u003e";
#if U'\U0000003e'
#endif
void *tU62 = U"\U0000003e";
#if U'\u003f'
#endif
void *tu63 = U"\u003f";
#if U'\U0000003f'
#endif
void *tU63 = U"\U0000003f";
#if U'\u0040'
#endif
void *tu64 = U"\u0040";
#if U'\U00000040'
#endif
void *tU64 = U"\U00000040";
#if U'\u0041'
#endif
void *tu65 = U"\u0041";
#if U'\U00000041'
#endif
void *tU65 = U"\U00000041";
#if U'\u0042'
#endif
void *tu66 = U"\u0042";
#if U'\U00000042'
#endif
void *tU66 = U"\U00000042";
#if U'\u0043'
#endif
void *tu67 = U"\u0043";
#if U'\U00000043'
#endif
void *tU67 = U"\U00000043";
#if U'\u0044'
#endif
void *tu68 = U"\u0044";
#if U'\U00000044'
#endif
void *tU68 = U"\U00000044";
#if U'\u0045'
#endif
void *tu69 = U"\u0045";
#if U'\U00000045'
#endif
void *tU69 = U"\U00000045";
#if U'\u0046'
#endif
void *tu70 = U"\u0046";
#if U'\U00000046'
#endif
void *tU70 = U"\U00000046";
#if U'\u0047'
#endif
void *tu71 = U"\u0047";
#if U'\U00000047'
#endif
void *tU71 = U"\U00000047";
#if U'\u0048'
#endif
void *tu72 = U"\u0048";
#if U'\U00000048'
#endif
void *tU72 = U"\U00000048";
#if U'\u0049'
#endif
void *tu73 = U"\u0049";
#if U'\U00000049'
#endif
void *tU73 = U"\U00000049";
#if U'\u004a'
#endif
void *tu74 = U"\u004a";
#if U'\U0000004a'
#endif
void *tU74 = U"\U0000004a";
#if U'\u004b'
#endif
void *tu75 = U"\u004b";
#if U'\U0000004b'
#endif
void *tU75 = U"\U0000004b";
#if U'\u004c'
#endif
void *tu76 = U"\u004c";
#if U'\U0000004c'
#endif
void *tU76 = U"\U0000004c";
#if U'\u004d'
#endif
void *tu77 = U"\u004d";
#if U'\U0000004d'
#endif
void *tU77 = U"\U0000004d";
#if U'\u004e'
#endif
void *tu78 = U"\u004e";
#if U'\U0000004e'
#endif
void *tU78 = U"\U0000004e";
#if U'\u004f'
#endif
void *tu79 = U"\u004f";
#if U'\U0000004f'
#endif
void *tU79 = U"\U0000004f";
#if U'\u0050'
#endif
void *tu80 = U"\u0050";
#if U'\U00000050'
#endif
void *tU80 = U"\U00000050";
#if U'\u0051'
#endif
void *tu81 = U"\u0051";
#if U'\U00000051'
#endif
void *tU81 = U"\U00000051";
#if U'\u0052'
#endif
void *tu82 = U"\u0052";
#if U'\U00000052'
#endif
void *tU82 = U"\U00000052";
#if U'\u0053'
#endif
void *tu83 = U"\u0053";
#if U'\U00000053'
#endif
void *tU83 = U"\U00000053";
#if U'\u0054'
#endif
void *tu84 = U"\u0054";
#if U'\U00000054'
#endif
void *tU84 = U"\U00000054";
#if U'\u0055'
#endif
void *tu85 = U"\u0055";
#if U'\U00000055'
#endif
void *tU85 = U"\U00000055";
#if U'\u0056'
#endif
void *tu86 = U"\u0056";
#if U'\U00000056'
#endif
void *tU86 = U"\U00000056";
#if U'\u0057'
#endif
void *tu87 = U"\u0057";
#if U'\U00000057'
#endif
void *tU87 = U"\U00000057";
#if U'\u0058'
#endif
void *tu88 = U"\u0058";
#if U'\U00000058'
#endif
void *tU88 = U"\U00000058";
#if U'\u0059'
#endif
void *tu89 = U"\u0059";
#if U'\U00000059'
#endif
void *tU89 = U"\U00000059";
#if U'\u005a'
#endif
void *tu90 = U"\u005a";
#if U'\U0000005a'
#endif
void *tU90 = U"\U0000005a";
#if U'\u005b'
#endif
void *tu91 = U"\u005b";
#if U'\U0000005b'
#endif
void *tU91 = U"\U0000005b";
#if U'\u005c'
#endif
void *tu92 = U"\u005c";
#if U'\U0000005c'
#endif
void *tU92 = U"\U0000005c";
#if U'\u005d'
#endif
void *tu93 = U"\u005d";
#if U'\U0000005d'
#endif
void *tU93 = U"\U0000005d";
#if U'\u005e'
#endif
void *tu94 = U"\u005e";
#if U'\U0000005e'
#endif
void *tU94 = U"\U0000005e";
#if U'\u005f'
#endif
void *tu95 = U"\u005f";
#if U'\U0000005f'
#endif
void *tU95 = U"\U0000005f";
#if U'\u0060'
#endif
void *tu96 = U"\u0060";
#if U'\U00000060'
#endif
void *tU96 = U"\U00000060";
#if U'\u0061'
#endif
void *tu97 = U"\u0061";
#if U'\U00000061'
#endif
void *tU97 = U"\U00000061";
#if U'\u0062'
#endif
void *tu98 = U"\u0062";
#if U'\U00000062'
#endif
void *tU98 = U"\U00000062";
#if U'\u0063'
#endif
void *tu99 = U"\u0063";
#if U'\U00000063'
#endif
void *tU99 = U"\U00000063";
#if U'\u0064'
#endif
void *tu100 = U"\u0064";
#if U'\U00000064'
#endif
void *tU100 = U"\U00000064";
#if U'\u0065'
#endif
void *tu101 = U"\u0065";
#if U'\U00000065'
#endif
void *tU101 = U"\U00000065";
#if U'\u0066'
#endif
void *tu102 = U"\u0066";
#if U'\U00000066'
#endif
void *tU102 = U"\U00000066";
#if U'\u0067'
#endif
void *tu103 = U"\u0067";
#if U'\U00000067'
#endif
void *tU103 = U"\U00000067";
#if U'\u0068'
#endif
void *tu104 = U"\u0068";
#if U'\U00000068'
#endif
void *tU104 = U"\U00000068";
#if U'\u0069'
#endif
void *tu105 = U"\u0069";
#if U'\U00000069'
#endif
void *tU105 = U"\U00000069";
#if U'\u006a'
#endif
void *tu106 = U"\u006a";
#if U'\U0000006a'
#endif
void *tU106 = U"\U0000006a";
#if U'\u006b'
#endif
void *tu107 = U"\u006b";
#if U'\U0000006b'
#endif
void *tU107 = U"\U0000006b";
#if U'\u006c'
#endif
void *tu108 = U"\u006c";
#if U'\U0000006c'
#endif
void *tU108 = U"\U0000006c";
#if U'\u006d'
#endif
void *tu109 = U"\u006d";
#if U'\U0000006d'
#endif
void *tU109 = U"\U0000006d";
#if U'\u006e'
#endif
void *tu110 = U"\u006e";
#if U'\U0000006e'
#endif
void *tU110 = U"\U0000006e";
#if U'\u006f'
#endif
void *tu111 = U"\u006f";
#if U'\U0000006f'
#endif
void *tU111 = U"\U0000006f";
#if U'\u0070'
#endif
void *tu112 = U"\u0070";
#if U'\U00000070'
#endif
void *tU112 = U"\U00000070";
#if U'\u0071'
#endif
void *tu113 = U"\u0071";
#if U'\U00000071'
#endif
void *tU113 = U"\U00000071";
#if U'\u0072'
#endif
void *tu114 = U"\u0072";
#if U'\U00000072'
#endif
void *tU114 = U"\U00000072";
#if U'\u0073'
#endif
void *tu115 = U"\u0073";
#if U'\U00000073'
#endif
void *tU115 = U"\U00000073";
#if U'\u0074'
#endif
void *tu116 = U"\u0074";
#if U'\U00000074'
#endif
void *tU116 = U"\U00000074";
#if U'\u0075'
#endif
void *tu117 = U"\u0075";
#if U'\U00000075'
#endif
void *tU117 = U"\U00000075";
#if U'\u0076'
#endif
void *tu118 = U"\u0076";
#if U'\U00000076'
#endif
void *tU118 = U"\U00000076";
#if U'\u0077'
#endif
void *tu119 = U"\u0077";
#if U'\U00000077'
#endif
void *tU119 = U"\U00000077";
#if U'\u0078'
#endif
void *tu120 = U"\u0078";
#if U'\U00000078'
#endif
void *tU120 = U"\U00000078";
#if U'\u0079'
#endif
void *tu121 = U"\u0079";
#if U'\U00000079'
#endif
void *tU121 = U"\U00000079";
#if U'\u007a'
#endif
void *tu122 = U"\u007a";
#if U'\U0000007a'
#endif
void *tU122 = U"\U0000007a";
#if U'\u007b'
#endif
void *tu123 = U"\u007b";
#if U'\U0000007b'
#endif
void *tU123 = U"\U0000007b";
#if U'\u007c'
#endif
void *tu124 = U"\u007c";
#if U'\U0000007c'
#endif
void *tU124 = U"\U0000007c";
#if U'\u007d'
#endif
void *tu125 = U"\u007d";
#if U'\U0000007d'
#endif
void *tU125 = U"\U0000007d";
#if U'\u007e'
#endif
void *tu126 = U"\u007e";
#if U'\U0000007e'
#endif
void *tU126 = U"\U0000007e";
#if U'\u007f'
#endif
void *tu127 = U"\u007f";
#if U'\U0000007f'
#endif
void *tU127 = U"\U0000007f";
#if U'\u0080'
#endif
void *tu128 = U"\u0080";
#if U'\U00000080'
#endif
void *tU128 = U"\U00000080";
#if U'\u0081'
#endif
void *tu129 = U"\u0081";
#if U'\U00000081'
#endif
void *tU129 = U"\U00000081";
#if U'\u0082'
#endif
void *tu130 = U"\u0082";
#if U'\U00000082'
#endif
void *tU130 = U"\U00000082";
#if U'\u0083'
#endif
void *tu131 = U"\u0083";
#if U'\U00000083'
#endif
void *tU131 = U"\U00000083";
#if U'\u0084'
#endif
void *tu132 = U"\u0084";
#if U'\U00000084'
#endif
void *tU132 = U"\U00000084";
#if U'\u0085'
#endif
void *tu133 = U"\u0085";
#if U'\U00000085'
#endif
void *tU133 = U"\U00000085";
#if U'\u0086'
#endif
void *tu134 = U"\u0086";
#if U'\U00000086'
#endif
void *tU134 = U"\U00000086";
#if U'\u0087'
#endif
void *tu135 = U"\u0087";
#if U'\U00000087'
#endif
void *tU135 = U"\U00000087";
#if U'\u0088'
#endif
void *tu136 = U"\u0088";
#if U'\U00000088'
#endif
void *tU136 = U"\U00000088";
#if U'\u0089'
#endif
void *tu137 = U"\u0089";
#if U'\U00000089'
#endif
void *tU137 = U"\U00000089";
#if U'\u008a'
#endif
void *tu138 = U"\u008a";
#if U'\U0000008a'
#endif
void *tU138 = U"\U0000008a";
#if U'\u008b'
#endif
void *tu139 = U"\u008b";
#if U'\U0000008b'
#endif
void *tU139 = U"\U0000008b";
#if U'\u008c'
#endif
void *tu140 = U"\u008c";
#if U'\U0000008c'
#endif
void *tU140 = U"\U0000008c";
#if U'\u008d'
#endif
void *tu141 = U"\u008d";
#if U'\U0000008d'
#endif
void *tU141 = U"\U0000008d";
#if U'\u008e'
#endif
void *tu142 = U"\u008e";
#if U'\U0000008e'
#endif
void *tU142 = U"\U0000008e";
#if U'\u008f'
#endif
void *tu143 = U"\u008f";
#if U'\U0000008f'
#endif
void *tU143 = U"\U0000008f";
#if U'\u0090'
#endif
void *tu144 = U"\u0090";
#if U'\U00000090'
#endif
void *tU144 = U"\U00000090";
#if U'\u0091'
#endif
void *tu145 = U"\u0091";
#if U'\U00000091'
#endif
void *tU145 = U"\U00000091";
#if U'\u0092'
#endif
void *tu146 = U"\u0092";
#if U'\U00000092'
#endif
void *tU146 = U"\U00000092";
#if U'\u0093'
#endif
void *tu147 = U"\u0093";
#if U'\U00000093'
#endif
void *tU147 = U"\U00000093";
#if U'\u0094'
#endif
void *tu148 = U"\u0094";
#if U'\U00000094'
#endif
void *tU148 = U"\U00000094";
#if U'\u0095'
#endif
void *tu149 = U"\u0095";
#if U'\U00000095'
#endif
void *tU149 = U"\U00000095";
#if U'\u0096'
#endif
void *tu150 = U"\u0096";
#if U'\U00000096'
#endif
void *tU150 = U"\U00000096";
#if U'\u0097'
#endif
void *tu151 = U"\u0097";
#if U'\U00000097'
#endif
void *tU151 = U"\U00000097";
#if U'\u0098'
#endif
void *tu152 = U"\u0098";
#if U'\U00000098'
#endif
void *tU152 = U"\U00000098";
#if U'\u0099'
#endif
void *tu153 = U"\u0099";
#if U'\U00000099'
#endif
void *tU153 = U"\U00000099";
#if U'\u009a'
#endif
void *tu154 = U"\u009a";
#if U'\U0000009a'
#endif
void *tU154 = U"\U0000009a";
#if U'\u009b'
#endif
void *tu155 = U"\u009b";
#if U'\U0000009b'
#endif
void *tU155 = U"\U0000009b";
#if U'\u009c'
#endif
void *tu156 = U"\u009c";
#if U'\U0000009c'
#endif
void *tU156 = U"\U0000009c";
#if U'\u009d'
#endif
void *tu157 = U"\u009d";
#if U'\U0000009d'
#endif
void *tU157 = U"\U0000009d";
#if U'\u009e'
#endif
void *tu158 = U"\u009e";
#if U'\U0000009e'
#endif
void *tU158 = U"\U0000009e";
#if U'\u009f'
#endif
void *tu159 = U"\u009f";
#if U'\U0000009f'
#endif
void *tU159 = U"\U0000009f";
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

/* It's less clear whether C requires this to be rejected before C23, but GCC
   chooses to do so.  */
#if U'\U00110000' /* { dg-warning "is outside the UCS codespace" } */
#endif
void *tU110000 = U"\U00110000"; /* { dg-warning "is outside the UCS codespace" } */

#if U'\Uffffffff' /* { dg-error "is not a valid universal character" } */
#endif
void *tUffffffff = U"\Uffffffff"; /* { dg-error "is not a valid universal character" } */
