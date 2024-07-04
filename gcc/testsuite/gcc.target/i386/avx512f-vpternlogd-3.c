/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

typedef unsigned int V __attribute__ ((__vector_size__ (16)));

V foo_0_1(V a, V b, V c) { return (V){0,0,0,0}; }

V foo_1_1(V a, V b, V c) { return ~((b|a)|c); }

V foo_2_1(V a, V b, V c) { return (~(b|a))&c; }

V foo_3_1(V a, V b, V c) { return ~(b|a); }

V foo_4_1(V a, V b, V c) { return (~(c|a))&b; }

V foo_5_1(V a, V b, V c) { return ~(c|a); }

V foo_6_1(V a, V b, V c) { return (c^b)&~a; }

V foo_7_1(V a, V b, V c) { return ~((c&b)|a); }

V foo_8_1(V a, V b, V c) { return (~a&c)&b; }
V foo_8_2(V a, V b, V c) { return (~a&b)&c; }
V foo_8_3(V a, V b, V c) { return (c&b)&~a; }

V foo_9_1(V a, V b, V c) { return ~((c^b)|a); }

V foo_10_1(V a, V b, V c) { return ~a&c; }

V foo_11_1(V a, V b, V c) { return ~((~c&b)|a); }
V foo_11_2(V a, V b, V c) { return (~b|c)&~a; }

V foo_12_1(V a, V b, V c) { return ~a&b; }

V foo_13_1(V a, V b, V c) { return ~((~b&c)|a); }
V foo_13_2(V a, V b, V c) { return (~c|b)&~a; }

V foo_14_1(V a, V b, V c) { return (c|b)&~a; }

V foo_15_1(V a, V b, V c) { return ~a; }

V foo_16_1(V a, V b, V c) { return (~(c|b))&a; }

V foo_17_1(V a, V b, V c) { return ~(c|b); }

V foo_18_1(V a, V b, V c) { return (c^a)&~b; }

V foo_19_1(V a, V b, V c) { return ~((c&a)|b); }

V foo_20_1(V a, V b, V c) { return (b^a)&~c; }

V foo_21_1(V a, V b, V c) { return ~((b&a)|c); }

V foo_22_1(V a, V b, V c) { return ((b^a)|(c&b))^c; }
V foo_22_2(V a, V b, V c) { return ((c^a)|(c&b))^b; }
V foo_22_3(V a, V b, V c) { return ((c^b)|(c&a))^a; }
V foo_22_4(V a, V b, V c) { return ((b&a)|c)^(b|a); }
V foo_22_5(V a, V b, V c) { return ((c&a)|b)^(c|a); }
V foo_22_6(V a, V b, V c) { return ((c&b)|a)^(c|b); }

V foo_23_1(V a, V b, V c) { return ~(((b^a)&(c^a))^a); }
V foo_23_2(V a, V b, V c) { return ((b^a)&(c^a))^~a; }
V foo_23_3(V a, V b, V c) { return ((b^a)&(c^b))^~b; }
V foo_23_4(V a, V b, V c) { return ((b^a)&c)^(~(b&a)); }
V foo_23_5(V a, V b, V c) { return ((c^a)&(c^b))^~c; }
V foo_23_6(V a, V b, V c) { return ((c^a)&b)^(~(c&a)); }
V foo_23_7(V a, V b, V c) { return ((c^b)&a)^(~(c&b)); }
V foo_23_8(V a, V b, V c) { return (~((c^b)&a))^(c&b); }
V foo_23_9(V a, V b, V c) { return (~((c^a)&b))^(c&a); }
V foo_23_10(V a, V b, V c) { return (~((c^a)&(c^b)))^c; }
V foo_23_11(V a, V b, V c) { return (~((b^a)&c))^(b&a); }
V foo_23_12(V a, V b, V c) { return (~((b^a)&(c^b)))^b; }
V foo_23_13(V a, V b, V c) { return (~((b^a)&(c^a)))^a; }
V foo_23_14(V a, V b, V c) { return ((~(b^a))|c)^(b|a); }
V foo_23_15(V a, V b, V c) { return ((~(c^a))|b)^(c|a); }
V foo_23_16(V a, V b, V c) { return ((~(c^b))|a)^(c|b); }

V foo_24_1(V a, V b, V c) { return (b^a)&(c^a); }

V foo_25_1(V a, V b, V c) { return ~((c^b)|(c&a)); }
V foo_25_2(V a, V b, V c) { return ((c&a)|~b)^c; }
V foo_25_3(V a, V b, V c) { return ((b&a)|~c)^b; }

V foo_26_1(V a, V b, V c) { return ((b&a)|c)^a; }

V foo_27_1(V a, V b, V c) { return ~(((b^a)&c)^b); }
V foo_27_2(V a, V b, V c) { return ((b^a)&c)^~b; }
V foo_27_3(V a, V b, V c) { return (~b|c)^(c&a); }
V foo_27_4(V a, V b, V c) { return (~((b^a)&c))^b; }
V foo_27_5(V a, V b, V c) { return ((~(b^a))|c)^a; }
V foo_27_6(V a, V b, V c) { return (~c|a)^(c|b); }

V foo_28_1(V a, V b, V c) { return ((c&a)|b)^a; }

V foo_29_1(V a, V b, V c) { return ~(((c^a)&b)^c); }
V foo_29_2(V a, V b, V c) { return ((c^a)&b)^~c; }
V foo_29_3(V a, V b, V c) { return (~((c^a)&b))^c; }
V foo_29_4(V a, V b, V c) { return (~c|b)^(b&a); }
V foo_29_5(V a, V b, V c) { return ((~(c^a))|b)^a; }
V foo_29_6(V a, V b, V c) { return (~b|a)^(c|b); }

V foo_30_1(V a, V b, V c) { return (c|b)^a; }

V foo_31_1(V a, V b, V c) { return ~((c|b)&a); }

V foo_32_1(V a, V b, V c) { return (~b&c)&a; }
V foo_32_2(V a, V b, V c) { return (~b&a)&c; }
V foo_32_3(V a, V b, V c) { return (c&a)&~b; }

V foo_33_1(V a, V b, V c) { return ~((c^a)|b); }

V foo_34_1(V a, V b, V c) { return ~b&c; }

V foo_35_1(V a, V b, V c) { return ~((~c&a)|b); }
V foo_35_2(V a, V b, V c) { return (~a|c)&~b; }

V foo_36_1(V a, V b, V c) { return (b^a)&(c^b); }

V foo_37_1(V a, V b, V c) { return ~((c^a)|(c&b)); }
V foo_37_2(V a, V b, V c) { return ((c&b)|~a)^c; }
V foo_37_3(V a, V b, V c) { return ((b&a)|~c)^a; }

V foo_38_1(V a, V b, V c) { return ((b&a)|c)^b; }

V foo_39_1(V a, V b, V c) { return ~(((b^a)&c)^a); }
V foo_39_2(V a, V b, V c) { return ((b^a)&c)^~a; }
V foo_39_3(V a, V b, V c) { return (~a|c)^(c&b); }
V foo_39_4(V a, V b, V c) { return ((~(b^a))|c)^b; }
V foo_39_5(V a, V b, V c) { return (~((b^a)&c))^a; }
V foo_39_6(V a, V b, V c) { return (~c|b)^(c|a); }

V foo_40_1(V a, V b, V c) { return (b^a)&c; }

V foo_41_1(V a, V b, V c) { return ~((((b&a)|c)^a)^b); }
V foo_41_2(V a, V b, V c) { return (((b&a)|c)^b)^~a; }
V foo_41_3(V a, V b, V c) { return (~((b&a)|c))^(b^a); }
V foo_41_4(V a, V b, V c) { return (((b&a)|c)^a)^~b; }
V foo_41_5(V a, V b, V c) { return ((b&a)|c)^(~(b^a)); }
V foo_41_6(V a, V b, V c) { return (~(((b&a)|c)^a))^b; }
V foo_41_7(V a, V b, V c) { return ((b&a)|~c)^(b|a); }
V foo_41_8(V a, V b, V c) { return (~(((b&a)|c)^b))^a; }

V foo_42_1(V a, V b, V c) { return (~(b&a))&c; }

V foo_43_1(V a, V b, V c) { return ~(((b^a)&(c^a))^b); }
V foo_43_2(V a, V b, V c) { return ((b^a)&c)|(~(b|a)); }
V foo_43_3(V a, V b, V c) { return ((b^a)&c)^(~(b|a)); }
V foo_43_4(V a, V b, V c) { return ((b^a)&(c^b))^~a; }
V foo_43_5(V a, V b, V c) { return ((b^a)&(c^a))^~b; }
V foo_43_6(V a, V b, V c) { return ((b^a)|(c^a))^~c; }
V foo_43_7(V a, V b, V c) { return (~((b^a)|(c^a)))^c; }
V foo_43_8(V a, V b, V c) { return ((~(b^a))|c)^(b&a); }
V foo_43_9(V a, V b, V c) { return (~((b^a)&(c^a)))^b; }
V foo_43_10(V a, V b, V c) { return (~((b^a)&c))^(b|a); }
V foo_43_11(V a, V b, V c) { return (~((b^a)&(c^b)))^a; }
V foo_43_12(V a, V b, V c) { return ((c^b)|a)^(~c|b); }
V foo_43_13(V a, V b, V c) { return ((c^a)|b)^(~c|a); }

V foo_44_1(V a, V b, V c) { return (b^a)&(c|b); }
V foo_44_2(V a, V b, V c) { return ((c|b)&a)^b; }

V foo_45_1(V a, V b, V c) { return (~c|b)^a; }

V foo_46_1(V a, V b, V c) { return (b&a)^(c|b); }
V foo_46_2(V a, V b, V c) { return ((c^a)|b)^a; }

V foo_47_1(V a, V b, V c) { return ~((~c|b)&a); }
V foo_47_2(V a, V b, V c) { return (~b&c)|~a; }

V foo_48_1(V a, V b, V c) { return ~b&a; }

V foo_49_1(V a, V b, V c) { return ~((~a&c)|b); }
V foo_49_2(V a, V b, V c) { return (~c|a)&~b; }

V foo_50_1(V a, V b, V c) { return (c|a)&~b; }

V foo_51_1(V a, V b, V c) { return ~b; }

V foo_52_1(V a, V b, V c) { return ((c&b)|a)^b; }

V foo_53_1(V a, V b, V c) { return ~(((c^b)&a)^c); }
V foo_53_2(V a, V b, V c) { return ((c^b)&a)^~c; }
V foo_53_3(V a, V b, V c) { return (~((c^b)&a))^c; }
V foo_53_4(V a, V b, V c) { return (~c|a)^(b&a); }
V foo_53_5(V a, V b, V c) { return ((~(c^b))|a)^b; }
V foo_53_6(V a, V b, V c) { return (~a|b)^(c|a); }

V foo_54_1(V a, V b, V c) { return (c|a)^b; }

V foo_55_1(V a, V b, V c) { return ~((c|a)&b); }

V foo_56_1(V a, V b, V c) { return (b^a)&(c|a); }
V foo_56_2(V a, V b, V c) { return ((c|a)&b)^a; }

V foo_57_1(V a, V b, V c) { return (~c|a)^b; }

V foo_58_1(V a, V b, V c) { return (b&a)^(c|a); }
V foo_58_2(V a, V b, V c) { return ((c^b)|a)^b; }

V foo_59_1(V a, V b, V c) { return ~((~c|a)&b); }
V foo_59_2(V a, V b, V c) { return (~a&c)|~b; }

V foo_60_1(V a, V b, V c) { return b^a; }

V foo_61_1(V a, V b, V c) { return (~(c|a))|(b^a); }
V foo_61_2(V a, V b, V c) { return (~(c|b))|(b^a); }
V foo_61_3(V a, V b, V c) { return ((~(c|b))|a)^b; }
V foo_61_4(V a, V b, V c) { return ((~(c|a))|b)^a; }

V foo_62_1(V a, V b, V c) { return (~a&c)|(b^a); }
V foo_62_2(V a, V b, V c) { return (~b&c)|(b^a); }
V foo_62_3(V a, V b, V c) { return ((~b&c)|a)^b; }
V foo_62_4(V a, V b, V c) { return ((~a&c)|b)^a; }

V foo_63_1(V a, V b, V c) { return ~(b&a); }

V foo_64_1(V a, V b, V c) { return (~c&b)&a; }
V foo_64_2(V a, V b, V c) { return (~c&a)&b; }
V foo_64_3(V a, V b, V c) { return (b&a)&~c; }

V foo_65_1(V a, V b, V c) { return ~((b^a)|c); }

V foo_66_1(V a, V b, V c) { return (c^a)&(c^b); }

V foo_67_1(V a, V b, V c) { return ~((b^a)|(c&b)); }
V foo_67_2(V a, V b, V c) { return ((c&b)|~a)^b; }
V foo_67_3(V a, V b, V c) { return ((c&a)|~b)^a; }

V foo_68_1(V a, V b, V c) { return ~c&b; }

V foo_69_1(V a, V b, V c) { return ~((~b&a)|c); }
V foo_69_2(V a, V b, V c) { return (~a|b)&~c; }

V foo_70_1(V a, V b, V c) { return ((c&a)|b)^c; }

V foo_71_1(V a, V b, V c) { return ~(((c^a)&b)^a); }
V foo_71_2(V a, V b, V c) { return ((c^a)&b)^~a; }
V foo_71_3(V a, V b, V c) { return (~a|b)^(c&b); }
V foo_71_4(V a, V b, V c) { return ((~(c^a))|b)^c; }
V foo_71_5(V a, V b, V c) { return (~((c^a)&b))^a; }
V foo_71_6(V a, V b, V c) { return (~b|c)^(b|a); }

V foo_72_1(V a, V b, V c) { return (c^a)&b; }

V foo_73_1(V a, V b, V c) { return ~((((c&a)|b)^a)^c); }
V foo_73_2(V a, V b, V c) { return (((c&a)|b)^c)^~a; }
V foo_73_3(V a, V b, V c) { return (~((c&a)|b))^(c^a); }
V foo_73_4(V a, V b, V c) { return (((c&a)|b)^a)^~c; }
V foo_73_5(V a, V b, V c) { return ((c&a)|b)^(~(c^a)); }
V foo_73_6(V a, V b, V c) { return (~(((c&a)|b)^a))^c; }
V foo_73_7(V a, V b, V c) { return ((c&a)|~b)^(c|a); }
V foo_73_8(V a, V b, V c) { return (~(((c&a)|b)^c))^a; }

V foo_74_1(V a, V b, V c) { return (c^a)&(c|b); }
V foo_74_2(V a, V b, V c) { return ((c|b)&a)^c; }

V foo_75_1(V a, V b, V c) { return (~b|c)^a; }

V foo_76_1(V a, V b, V c) { return (~(c&a))&b; }

V foo_77_1(V a, V b, V c) { return ~(((b^a)&(c^a))^c); }
V foo_77_2(V a, V b, V c) { return ((c^a)&b)|(~(c|a)); }
V foo_77_3(V a, V b, V c) { return ((c^a)&b)^(~(c|a)); }
V foo_77_4(V a, V b, V c) { return ((c^a)&(c^b))^~a; }
V foo_77_5(V a, V b, V c) { return ((b^a)&(c^a))^~c; }
V foo_77_6(V a, V b, V c) { return ((b^a)|(c^a))^~b; }
V foo_77_7(V a, V b, V c) { return (~((b^a)|(c^a)))^b; }
V foo_77_8(V a, V b, V c) { return ((~(c^a))|b)^(c&a); }
V foo_77_9(V a, V b, V c) { return (~((b^a)&(c^a)))^c; }
V foo_77_10(V a, V b, V c) { return (~((c^a)&b))^(c|a); }
V foo_77_11(V a, V b, V c) { return ((c^b)|a)^(~b|c); }
V foo_77_12(V a, V b, V c) { return (~((c^a)&(c^b)))^a; }
V foo_77_13(V a, V b, V c) { return ((b^a)|c)^(~b|a); }

V foo_78_1(V a, V b, V c) { return (c&a)^(c|b); }
V foo_78_2(V a, V b, V c) { return ((b^a)|c)^a; }

V foo_79_1(V a, V b, V c) { return ~((~b|c)&a); }
V foo_79_2(V a, V b, V c) { return (~c&b)|~a; }

V foo_80_1(V a, V b, V c) { return ~c&a; }

V foo_81_1(V a, V b, V c) { return ~((~a&b)|c); }
V foo_81_2(V a, V b, V c) { return (~b|a)&~c; }

V foo_82_1(V a, V b, V c) { return ((c&b)|a)^c; }

V foo_83_1(V a, V b, V c) { return ~(((c^b)&a)^b); }
V foo_83_2(V a, V b, V c) { return ((c^b)&a)^~b; }
V foo_83_3(V a, V b, V c) { return (~((c^b)&a))^b; }
V foo_83_4(V a, V b, V c) { return (~b|a)^(c&a); }
V foo_83_5(V a, V b, V c) { return ((~(c^b))|a)^c; }
V foo_83_6(V a, V b, V c) { return (~a|c)^(b|a); }

V foo_84_1(V a, V b, V c) { return (b|a)&~c; }

V foo_85_1(V a, V b, V c) { return ~c; }

V foo_86_1(V a, V b, V c) { return (b|a)^c; }

V foo_87_1(V a, V b, V c) { return ~((b|a)&c); }

V foo_88_1(V a, V b, V c) { return (c^a)&(b|a); }
V foo_88_2(V a, V b, V c) { return ((b|a)&c)^a; }

V foo_89_1(V a, V b, V c) { return (~b|a)^c; }

V foo_90_1(V a, V b, V c) { return c^a; }

V foo_91_1(V a, V b, V c) { return (~(b|a))|(c^a); }
V foo_91_2(V a, V b, V c) { return (~(c|b))|(c^a); }
V foo_91_3(V a, V b, V c) { return ((~(c|b))|a)^c; }
V foo_91_4(V a, V b, V c) { return ((~(b|a))|c)^a; }

V foo_92_1(V a, V b, V c) { return (c&a)^(b|a); }
V foo_92_2(V a, V b, V c) { return ((c^b)|a)^c; }

V foo_93_1(V a, V b, V c) { return ~((~b|a)&c); }
V foo_93_2(V a, V b, V c) { return (~a&b)|~c; }

V foo_94_1(V a, V b, V c) { return (~a&b)|(c^a); }
V foo_94_2(V a, V b, V c) { return (~c&b)|(c^a); }
V foo_94_3(V a, V b, V c) { return ((~c&b)|a)^c; }
V foo_94_4(V a, V b, V c) { return ((~a&b)|c)^a; }

V foo_95_1(V a, V b, V c) { return ~(c&a); }

V foo_96_1(V a, V b, V c) { return (c^b)&a; }

V foo_97_1(V a, V b, V c) { return ~(((c|b)^a)|(c&b)); }
V foo_97_2(V a, V b, V c) { return (~((c&b)|a))^(c^b); }
V foo_97_3(V a, V b, V c) { return (((c&b)|a)^c)^~b; }
V foo_97_4(V a, V b, V c) { return (((c&b)|a)^b)^~c; }
V foo_97_5(V a, V b, V c) { return ((c&b)|~a)^(c|b); }
V foo_97_6(V a, V b, V c) { return ((c&b)|a)^(~(c^b)); }
V foo_97_7(V a, V b, V c) { return (~(((c&b)|a)^b))^c; }
V foo_97_8(V a, V b, V c) { return (~(((c&b)|a)^c))^b; }

V foo_98_1(V a, V b, V c) { return (c^b)&(c|a); }
V foo_98_2(V a, V b, V c) { return ((c|a)&b)^c; }

V foo_99_1(V a, V b, V c) { return (~a|c)^b; }

V foo_100_1(V a, V b, V c) { return (c^b)&(b|a); }
V foo_100_2(V a, V b, V c) { return ((b|a)&c)^b; }

V foo_101_1(V a, V b, V c) { return (~a|b)^c; }

V foo_102_1(V a, V b, V c) { return c^b; }

V foo_103_1(V a, V b, V c) { return (~(b|a))|(c^b); }
V foo_103_2(V a, V b, V c) { return (~(c|a))|(c^b); }
V foo_103_3(V a, V b, V c) { return ((~(c|a))|b)^c; }
V foo_103_4(V a, V b, V c) { return ((~(b|a))|c)^b; }

V foo_104_1(V a, V b, V c) { return ((b&a)^c)&(b|a); }
V foo_104_2(V a, V b, V c) { return ((c&a)^b)&(c|a); }
V foo_104_3(V a, V b, V c) { return ((c&b)^a)&(c|b); }
V foo_104_4(V a, V b, V c) { return ((c|b)&a)^(c&b); }
V foo_104_5(V a, V b, V c) { return ((c|a)&b)^(c&a); }
V foo_104_6(V a, V b, V c) { return ((b|a)&c)^(b&a); }

V foo_105_1(V a, V b, V c) { return ~((b^a)^c); }
V foo_105_2(V a, V b, V c) { return (c^b)^~a; }
V foo_105_3(V a, V b, V c) { return (c^a)^~b; }
V foo_105_4(V a, V b, V c) { return (b^a)^~c; }
V foo_105_5(V a, V b, V c) { return (~(c^b))^a; }
V foo_105_6(V a, V b, V c) { return (~(c^a))^b; }
V foo_105_7(V a, V b, V c) { return (~(b^a))^c; }

V foo_106_1(V a, V b, V c) { return (b&a)^c; }

V foo_107_1(V a, V b, V c) { return ~(((b|a)&c)^(b^a)); }
V foo_107_2(V a, V b, V c) { return ((b&a)^c)|(~(b|a)); }
V foo_107_3(V a, V b, V c) { return ((c^b)&(b|a))^~a; }
V foo_107_4(V a, V b, V c) { return ((c^a)&(b|a))^~b; }
V foo_107_5(V a, V b, V c) { return (~((b|a)&c))^(b^a); }
V foo_107_6(V a, V b, V c) { return (~((c^b)&(b|a)))^a; }
V foo_107_7(V a, V b, V c) { return (~((c^a)&(b|a)))^b; }
V foo_107_8(V a, V b, V c) { return ((b|a)&c)^(~(b^a)); }
V foo_107_9(V a, V b, V c) { return ((~(b|a))|c)^(b&a); }

V foo_108_1(V a, V b, V c) { return (c&a)^b; }

V foo_109_1(V a, V b, V c) { return ~(((b^a)&(c|a))^c); }
V foo_109_2(V a, V b, V c) { return ((c&a)^b)|(~(c|a)); }
V foo_109_3(V a, V b, V c) { return ((c^b)&(c|a))^~a; }
V foo_109_4(V a, V b, V c) { return (~((c|a)&b))^(c^a); }
V foo_109_5(V a, V b, V c) { return ((b^a)&(c|a))^~c; }
V foo_109_6(V a, V b, V c) { return (~((c^b)&(c|a)))^a; }
V foo_109_7(V a, V b, V c) { return ((~(c|a))|b)^(c&a); }
V foo_109_8(V a, V b, V c) { return ((c|a)&b)^(~(c^a)); }
V foo_109_9(V a, V b, V c) { return (~((b^a)&(c|a)))^c; }

V foo_110_1(V a, V b, V c) { return (~a&c)|(c^b); }
V foo_110_2(V a, V b, V c) { return (~a&b)|(c^b); }
V foo_110_3(V a, V b, V c) { return ((~b|a)&c)^b; }
V foo_110_4(V a, V b, V c) { return ((~c|a)&b)^c; }

V foo_111_1(V a, V b, V c) { return (c^b)|~a; }

V foo_112_1(V a, V b, V c) { return (~(c&b))&a; }

V foo_113_1(V a, V b, V c) { return ~(((b^a)&(c^b))^c); }
V foo_113_2(V a, V b, V c) { return ((b^a)|(c^a))^~a; }
V foo_113_3(V a, V b, V c) { return ((c^b)&a)|(~(c|b)); }
V foo_113_4(V a, V b, V c) { return ((c^b)&a)^(~(c|b)); }
V foo_113_5(V a, V b, V c) { return ((b^a)&(c^b))^~c; }
V foo_113_6(V a, V b, V c) { return ((c^a)&(c^b))^~b; }
V foo_113_7(V a, V b, V c) { return (~((b^a)|(c^a)))^a; }
V foo_113_8(V a, V b, V c) { return ((~(c^b))|a)^(c&b); }
V foo_113_9(V a, V b, V c) { return (~((c^b)&a))^(c|b); }
V foo_113_10(V a, V b, V c) { return (~((b^a)&(c^b)))^c; }
V foo_113_11(V a, V b, V c) { return ((c^a)|b)^(~a|c); }
V foo_113_12(V a, V b, V c) { return (~((c^a)&(c^b)))^b; }
V foo_113_13(V a, V b, V c) { return ((b^a)|c)^(~a|b); }

V foo_114_1(V a, V b, V c) { return (c&b)^(c|a); }
V foo_114_2(V a, V b, V c) { return ((b^a)|c)^b; }

V foo_115_1(V a, V b, V c) { return ~((~a|c)&b); }
V foo_115_2(V a, V b, V c) { return (~c&a)|~b; }

V foo_116_1(V a, V b, V c) { return (c&b)^(b|a); }
V foo_116_2(V a, V b, V c) { return ((c^a)|b)^c; }

V foo_117_1(V a, V b, V c) { return ~((~a|b)&c); }
V foo_117_2(V a, V b, V c) { return (~b&a)|~c; }

V foo_118_1(V a, V b, V c) { return (~b&a)|(c^b); }
V foo_118_2(V a, V b, V c) { return (~c&a)|(c^b); }
V foo_118_3(V a, V b, V c) { return ((~c&a)|b)^c; }
V foo_118_4(V a, V b, V c) { return ((~b&a)|c)^b; }

V foo_119_1(V a, V b, V c) { return ~(c&b); }

V foo_120_1(V a, V b, V c) { return (c&b)^a; }

V foo_121_1(V a, V b, V c) { return ~(((b^a)&(c|b))^c); }
V foo_121_2(V a, V b, V c) { return ((c&b)^a)|(~(c|b)); }
V foo_121_3(V a, V b, V c) { return (~((c|b)&a))^(c^b); }
V foo_121_4(V a, V b, V c) { return ((b^a)&(c|b))^~c; }
V foo_121_5(V a, V b, V c) { return ((c^a)&(c|b))^~b; }
V foo_121_6(V a, V b, V c) { return ((~(c|b))|a)^(c&b); }
V foo_121_7(V a, V b, V c) { return ((c|b)&a)^(~(c^b)); }
V foo_121_8(V a, V b, V c) { return (~((b^a)&(c|b)))^c; }
V foo_121_9(V a, V b, V c) { return (~((c^a)&(c|b)))^b; }

V foo_122_1(V a, V b, V c) { return (~b&c)|(c^a); }
V foo_122_2(V a, V b, V c) { return (~b&a)|(c^a); }
V foo_122_3(V a, V b, V c) { return ((~a|b)&c)^a; }
V foo_122_4(V a, V b, V c) { return ((~c|b)&a)^c; }

V foo_123_1(V a, V b, V c) { return (c^a)|~b; }

V foo_124_1(V a, V b, V c) { return (~c&b)|(b^a); }
V foo_124_2(V a, V b, V c) { return (~c&a)|(b^a); }
V foo_124_3(V a, V b, V c) { return ((~a|c)&b)^a; }
V foo_124_4(V a, V b, V c) { return ((~b|c)&a)^b; }

V foo_125_1(V a, V b, V c) { return (b^a)|~c; }

V foo_126_1(V a, V b, V c) { return (b^a)|(c^a); }
V foo_126_2(V a, V b, V c) { return (b^a)|(c^b); }
V foo_126_3(V a, V b, V c) { return (c^a)|(c^b); }

V foo_127_1(V a, V b, V c) { return ~((c&b)&a); }

V foo_128_1(V a, V b, V c) { return (c&b)&a; }
V foo_128_2(V a, V b, V c) { return (c&a)&b; }
V foo_128_3(V a, V b, V c) { return (b&a)&c; }

V foo_129_1(V a, V b, V c) { return ~((b^a)|(c^a)); }

V foo_130_1(V a, V b, V c) { return (~(b^a))&c; }

V foo_131_1(V a, V b, V c) { return ~((~c&b)|(b^a)); }
V foo_131_2(V a, V b, V c) { return ((~a|c)&b)^~a; }
V foo_131_3(V a, V b, V c) { return ((~b|c)&a)^~b; }
V foo_131_4(V a, V b, V c) { return (~((~b|c)&a))^b; }
V foo_131_5(V a, V b, V c) { return (~((~a|c)&b))^a; }
V foo_131_6(V a, V b, V c) { return (~a|c)&(~(b^a)); }
V foo_131_7(V a, V b, V c) { return (~b|c)&(~(b^a)); }

V foo_132_1(V a, V b, V c) { return (~(c^a))&b; }

V foo_133_1(V a, V b, V c) { return ~((~b&c)|(c^a)); }
V foo_133_2(V a, V b, V c) { return ((~a|b)&c)^~a; }
V foo_133_3(V a, V b, V c) { return (~((~c|b)&a))^c; }
V foo_133_4(V a, V b, V c) { return ((~c|b)&a)^~c; }
V foo_133_5(V a, V b, V c) { return (~((~a|b)&c))^a; }
V foo_133_6(V a, V b, V c) { return (~(c^a))&(~a|b); }
V foo_133_7(V a, V b, V c) { return (~(c^a))&(~c|b); }

V foo_134_1(V a, V b, V c) { return ((b^a)&(c|b))^c; }
V foo_134_2(V a, V b, V c) { return ((c^a)&(c|b))^b; }
V foo_134_3(V a, V b, V c) { return ((c|b)&a)^(c^b); }
V foo_134_4(V a, V b, V c) { return ((b^a)^c)&(c|b); }

V foo_135_1(V a, V b, V c) { return ~((c&b)^a); }
V foo_135_2(V a, V b, V c) { return (c&b)^~a; }
V foo_135_3(V a, V b, V c) { return (~(c&b))^a; }

V foo_136_1(V a, V b, V c) { return c&b; }

V foo_137_1(V a, V b, V c) { return ~((~b&a)|(c^b)); }
V foo_137_2(V a, V b, V c) { return (~((~c&a)|b))^c; }
V foo_137_3(V a, V b, V c) { return ((~b&a)|c)^~b; }
V foo_137_4(V a, V b, V c) { return (~((~b&a)|c))^b; }
V foo_137_5(V a, V b, V c) { return ((~c&a)|b)^~c; }
V foo_137_6(V a, V b, V c) { return (~(c^b))&(~a|c); }
V foo_137_7(V a, V b, V c) { return (~(c^b))&(~a|b); }

V foo_138_1(V a, V b, V c) { return (~a|b)&c; }

V foo_139_1(V a, V b, V c) { return ~((c&b)^(b|a)); }
V foo_139_2(V a, V b, V c) { return (~(b|a))|(c&b); }
V foo_139_3(V a, V b, V c) { return (~(b|a))^(c&b); }
V foo_139_4(V a, V b, V c) { return (~((c^a)|b))^c; }
V foo_139_5(V a, V b, V c) { return ((c^a)|b)^~c; }
V foo_139_6(V a, V b, V c) { return (~(c&b))^(b|a); }
V foo_139_7(V a, V b, V c) { return ((c^a)|~b)^a; }

V foo_140_1(V a, V b, V c) { return (~a|c)&b; }

V foo_141_1(V a, V b, V c) { return ~((c&b)^(c|a)); }
V foo_141_2(V a, V b, V c) { return (~(c|a))|(c&b); }
V foo_141_3(V a, V b, V c) { return (~(c|a))^(c&b); }
V foo_141_4(V a, V b, V c) { return ((b^a)|c)^~b; }
V foo_141_5(V a, V b, V c) { return (~((b^a)|c))^b; }
V foo_141_6(V a, V b, V c) { return (~(c&b))^(c|a); }
V foo_141_7(V a, V b, V c) { return ((b^a)|~c)^a; }

V foo_142_1(V a, V b, V c) { return ((b^a)&(c^b))^c; }
V foo_142_2(V a, V b, V c) { return ((c^a)&(c^b))^b; }
V foo_142_3(V a, V b, V c) { return ((c^b)&a)^(c|b); }
V foo_142_4(V a, V b, V c) { return ((b^a)|(c^a))^a; }

V foo_143_1(V a, V b, V c) { return (c&b)|~a; }

V foo_144_1(V a, V b, V c) { return (~(c^b))&a; }

V foo_145_1(V a, V b, V c) { return ~((~a&c)|(c^b)); }
V foo_145_2(V a, V b, V c) { return ((~b|a)&c)^~b; }
V foo_145_3(V a, V b, V c) { return (~((~c|a)&b))^c; }
V foo_145_4(V a, V b, V c) { return ((~c|a)&b)^~c; }
V foo_145_5(V a, V b, V c) { return (~((~b|a)&c))^b; }
V foo_145_6(V a, V b, V c) { return (~(c^b))&(~b|a); }
V foo_145_7(V a, V b, V c) { return (~(c^b))&(~c|a); }

V foo_146_1(V a, V b, V c) { return ((b^a)&(c|a))^c; }
V foo_146_2(V a, V b, V c) { return ((c|a)&b)^(c^a); }
V foo_146_3(V a, V b, V c) { return ((c^b)&(c|a))^a; }
V foo_146_4(V a, V b, V c) { return ((b^a)^c)&(c|a); }

V foo_147_1(V a, V b, V c) { return ~((c&a)^b); }
V foo_147_2(V a, V b, V c) { return (c&a)^~b; }
V foo_147_3(V a, V b, V c) { return (~(c&a))^b; }

V foo_148_1(V a, V b, V c) { return ((b|a)&c)^(b^a); }
V foo_148_2(V a, V b, V c) { return ((c^a)&(b|a))^b; }
V foo_148_3(V a, V b, V c) { return ((c^b)&(b|a))^a; }
V foo_148_4(V a, V b, V c) { return ((b^a)^c)&(b|a); }

V foo_149_1(V a, V b, V c) { return ~((b&a)^c); }
V foo_149_2(V a, V b, V c) { return (~(b&a))^c; }
V foo_149_3(V a, V b, V c) { return (b&a)^~c; }

V foo_150_1(V a, V b, V c) { return (b^a)^c; }
V foo_150_2(V a, V b, V c) { return (c^a)^b; }
V foo_150_3(V a, V b, V c) { return (c^b)^a; }

V foo_151_1(V a, V b, V c) { return ~(((b&a)^c)&(b|a)); }
V foo_151_2(V a, V b, V c) { return ((b^a)^c)|(~(b|a)); }
V foo_151_3(V a, V b, V c) { return ((b^a)^c)|(~(c|a)); }
V foo_151_4(V a, V b, V c) { return ((b^a)^c)|(~(c|b)); }
V foo_151_5(V a, V b, V c) { return (~((c|b)&a))^(c&b); }
V foo_151_6(V a, V b, V c) { return (~((c|a)&b))^(c&a); }
V foo_151_7(V a, V b, V c) { return ((~(b|a))|c)^(b^a); }
V foo_151_8(V a, V b, V c) { return ((~(c|a))|(b^a))^c; }
V foo_151_9(V a, V b, V c) { return ((b|a)&c)^(~(b&a)); }
V foo_151_10(V a, V b, V c) { return (~((b|a)&c))^(b&a); }
V foo_151_11(V a, V b, V c) { return ((~(c|a))|b)^(c^a); }
V foo_151_12(V a, V b, V c) { return ((~(b|a))|(c^a))^b; }
V foo_151_13(V a, V b, V c) { return ((c|a)&b)^(~(c&a)); }
V foo_151_14(V a, V b, V c) { return ((~(c|b))|a)^(c^b); }
V foo_151_15(V a, V b, V c) { return ((~(b|a))|(c^b))^a; }
V foo_151_16(V a, V b, V c) { return ((c|b)&a)^(~(c&b)); }

V foo_152_1(V a, V b, V c) { return ((c|a)&~b)^c; }
V foo_152_2(V a, V b, V c) { return ((b|a)&~c)^b; }
V foo_152_3(V a, V b, V c) { return (~(c^b))&(c|a); }
V foo_152_4(V a, V b, V c) { return (~(c^b))&(b|a); }

V foo_153_1(V a, V b, V c) { return ~(c^b); }
V foo_153_2(V a, V b, V c) { return ~b^c; }
V foo_153_3(V a, V b, V c) { return ~c^b; }

V foo_154_1(V a, V b, V c) { return (~b&a)^c; }

V foo_155_1(V a, V b, V c) { return ~((c^b)&(b|a)); }
V foo_155_2(V a, V b, V c) { return ((b|a)&c)^~b; }
V foo_155_3(V a, V b, V c) { return (~((b|a)&c))^b; }

V foo_156_1(V a, V b, V c) { return (~c&a)^b; }

V foo_157_1(V a, V b, V c) { return ~((c^b)&(c|a)); }
V foo_157_2(V a, V b, V c) { return (~((c|a)&b))^c; }
V foo_157_3(V a, V b, V c) { return ((c|a)&b)^~c; }

V foo_158_1(V a, V b, V c) { return ((c|b)^a)|(c&b); }
V foo_158_2(V a, V b, V c) { return (((c&b)|a)^b)^c; }
V foo_158_3(V a, V b, V c) { return (((c&b)|a)^c)^b; }
V foo_158_4(V a, V b, V c) { return ((c&b)|a)^(c^b); }
V foo_158_5(V a, V b, V c) { return ((b^a)^c)|(c&b); }

V foo_159_1(V a, V b, V c) { return ~((c^b)&a); }

V foo_160_1(V a, V b, V c) { return c&a; }

V foo_161_1(V a, V b, V c) { return ~((~a&b)|(c^a)); }
V foo_161_2(V a, V b, V c) { return (~((~c&b)|a))^c; }
V foo_161_3(V a, V b, V c) { return ((~a&b)|c)^~a; }
V foo_161_4(V a, V b, V c) { return (~((~a&b)|c))^a; }
V foo_161_5(V a, V b, V c) { return ((~c&b)|a)^~c; }
V foo_161_6(V a, V b, V c) { return (~(c^a))&(~b|c); }
V foo_161_7(V a, V b, V c) { return (~(c^a))&(~b|a); }

V foo_162_1(V a, V b, V c) { return (~b|a)&c; }

V foo_163_1(V a, V b, V c) { return ~((c&a)^(b|a)); }
V foo_163_2(V a, V b, V c) { return (~(b|a))|(c&a); }
V foo_163_3(V a, V b, V c) { return (~(b|a))^(c&a); }
V foo_163_4(V a, V b, V c) { return (~((c^b)|a))^c; }
V foo_163_5(V a, V b, V c) { return ((c^b)|a)^~c; }
V foo_163_6(V a, V b, V c) { return (~(c&a))^(b|a); }
V foo_163_7(V a, V b, V c) { return ((c^b)|~a)^b; }

V foo_164_1(V a, V b, V c) { return ((c|b)&~a)^c; }
V foo_164_2(V a, V b, V c) { return ((b|a)&~c)^a; }
V foo_164_3(V a, V b, V c) { return (~(c^a))&(c|b); }
V foo_164_4(V a, V b, V c) { return (~(c^a))&(b|a); }

V foo_165_1(V a, V b, V c) { return ~(c^a); }
V foo_165_2(V a, V b, V c) { return ~a^c; }
V foo_165_3(V a, V b, V c) { return ~c^a; }

V foo_166_1(V a, V b, V c) { return (~a&b)^c; }

V foo_167_1(V a, V b, V c) { return ~((c^a)&(b|a)); }
V foo_167_2(V a, V b, V c) { return ((b|a)&c)^~a; }
V foo_167_3(V a, V b, V c) { return (~((b|a)&c))^a; }

V foo_168_1(V a, V b, V c) { return (b|a)&c; }

V foo_169_1(V a, V b, V c) { return ~((b|a)^c); }
V foo_169_2(V a, V b, V c) { return (~(b|a))^c; }
V foo_169_3(V a, V b, V c) { return (b|a)^~c; }

V foo_170_1(V a, V b, V c) { return c; }

V foo_171_1(V a, V b, V c) { return (~(b|a))|c; }

V foo_172_1(V a, V b, V c) { return ((c^b)&a)^b; }

V foo_173_1(V a, V b, V c) { return ~(((c&b)|a)^c); }
V foo_173_2(V a, V b, V c) { return (~((c&b)|a))^c; }
V foo_173_3(V a, V b, V c) { return ((c&b)|a)^~c; }
V foo_173_4(V a, V b, V c) { return (~(c^a))|(c&b); }

V foo_174_1(V a, V b, V c) { return (~a&b)|c; }

V foo_175_1(V a, V b, V c) { return ~a|c; }

V foo_176_1(V a, V b, V c) { return (~b|c)&a; }

V foo_177_1(V a, V b, V c) { return ~((c&a)^(c|b)); }
V foo_177_2(V a, V b, V c) { return ((b^a)|c)^~a; }
V foo_177_3(V a, V b, V c) { return (~(c|b))|(c&a); }
V foo_177_4(V a, V b, V c) { return (~(c|b))^(c&a); }
V foo_177_5(V a, V b, V c) { return (~((b^a)|c))^a; }
V foo_177_6(V a, V b, V c) { return (~(c&a))^(c|b); }
V foo_177_7(V a, V b, V c) { return ((b^a)|~c)^b; }

V foo_178_1(V a, V b, V c) { return ((b^a)&(c^a))^c; }
V foo_178_2(V a, V b, V c) { return ((c^a)&(c^b))^a; }
V foo_178_3(V a, V b, V c) { return ((c^a)&b)^(c|a); }
V foo_178_4(V a, V b, V c) { return ((b^a)|(c^a))^b; }

V foo_179_1(V a, V b, V c) { return (c&a)|~b; }

V foo_180_1(V a, V b, V c) { return (~c&b)^a; }

V foo_181_1(V a, V b, V c) { return ~((c^a)&(c|b)); }
V foo_181_2(V a, V b, V c) { return (~((c|b)&a))^c; }
V foo_181_3(V a, V b, V c) { return ((c|b)&a)^~c; }

V foo_182_1(V a, V b, V c) { return (((c&a)|b)^a)^c; }
V foo_182_2(V a, V b, V c) { return ((c|a)^b)|(c&a); }
V foo_182_3(V a, V b, V c) { return (((c&a)|b)^c)^a; }
V foo_182_4(V a, V b, V c) { return ((c&a)|b)^(c^a); }
V foo_182_5(V a, V b, V c) { return ((b^a)^c)|(c&a); }

V foo_183_1(V a, V b, V c) { return ~((c^a)&b); }

V foo_184_1(V a, V b, V c) { return ((c^a)&b)^a; }

V foo_185_1(V a, V b, V c) { return ~(((c&a)|b)^c); }
V foo_185_2(V a, V b, V c) { return (~((c&a)|b))^c; }
V foo_185_3(V a, V b, V c) { return ((c&a)|b)^~c; }
V foo_185_4(V a, V b, V c) { return (~(c^b))|(c&a); }

V foo_186_1(V a, V b, V c) { return (~b&a)|c; }

V foo_187_1(V a, V b, V c) { return ~b|c; }

V foo_188_1(V a, V b, V c) { return (b^a)|(c&b); }
V foo_188_2(V a, V b, V c) { return (b^a)|(c&a); }

V foo_189_1(V a, V b, V c) { return ~((c^a)&(c^b)); }
V foo_189_2(V a, V b, V c) { return (~(c^b))|(b^a); }
V foo_189_3(V a, V b, V c) { return (~(c^a))|(b^a); }

V foo_190_1(V a, V b, V c) { return (b^a)|c; }

V foo_191_1(V a, V b, V c) { return (~(b&a))|c; }

V foo_192_1(V a, V b, V c) { return b&a; }

V foo_193_1(V a, V b, V c) { return ~((~a&c)|(b^a)); }
V foo_193_2(V a, V b, V c) { return (~((~b&c)|a))^b; }
V foo_193_3(V a, V b, V c) { return ((~a&c)|b)^~a; }
V foo_193_4(V a, V b, V c) { return (~((~a&c)|b))^a; }
V foo_193_5(V a, V b, V c) { return ((~b&c)|a)^~b; }
V foo_193_6(V a, V b, V c) { return (~(b^a))&(~c|b); }
V foo_193_7(V a, V b, V c) { return (~(b^a))&(~c|a); }

V foo_194_1(V a, V b, V c) { return ((c|b)&~a)^b; }
V foo_194_2(V a, V b, V c) { return ((c|a)&~b)^a; }
V foo_194_3(V a, V b, V c) { return (~(b^a))&(c|b); }
V foo_194_4(V a, V b, V c) { return (~(b^a))&(c|a); }

V foo_195_1(V a, V b, V c) { return ~(b^a); }
V foo_195_2(V a, V b, V c) { return ~a^b; }
V foo_195_3(V a, V b, V c) { return ~b^a; }

V foo_196_1(V a, V b, V c) { return (~c|a)&b; }

V foo_197_1(V a, V b, V c) { return ~((b&a)^(c|a)); }
V foo_197_2(V a, V b, V c) { return (~(c|a))|(b&a); }
V foo_197_3(V a, V b, V c) { return (~(c|a))^(b&a); }
V foo_197_4(V a, V b, V c) { return (~((c^b)|a))^b; }
V foo_197_5(V a, V b, V c) { return ((c^b)|a)^~b; }
V foo_197_6(V a, V b, V c) { return (~(b&a))^(c|a); }
V foo_197_7(V a, V b, V c) { return ((c^b)|~a)^c; }

V foo_198_1(V a, V b, V c) { return (~a&c)^b; }

V foo_199_1(V a, V b, V c) { return ~((b^a)&(c|a)); }
V foo_199_2(V a, V b, V c) { return ((c|a)&b)^~a; }
V foo_199_3(V a, V b, V c) { return (~((c|a)&b))^a; }

V foo_200_1(V a, V b, V c) { return (c|a)&b; }

V foo_201_1(V a, V b, V c) { return ~((c|a)^b); }
V foo_201_2(V a, V b, V c) { return (~(c|a))^b; }
V foo_201_3(V a, V b, V c) { return (c|a)^~b; }

V foo_202_1(V a, V b, V c) { return ((c^b)&a)^c; }

V foo_203_1(V a, V b, V c) { return ~(((c&b)|a)^b); }
V foo_203_2(V a, V b, V c) { return (~((c&b)|a))^b; }
V foo_203_3(V a, V b, V c) { return ((c&b)|a)^~b; }
V foo_203_4(V a, V b, V c) { return (~(b^a))|(c&b); }

V foo_204_1(V a, V b, V c) { return b; }

V foo_205_1(V a, V b, V c) { return (~(c|a))|b; }

V foo_206_1(V a, V b, V c) { return (~a&c)|b; }

V foo_207_1(V a, V b, V c) { return ~a|b; }

V foo_208_1(V a, V b, V c) { return (~c|b)&a; }

V foo_209_1(V a, V b, V c) { return ~((b&a)^(c|b)); }
V foo_209_2(V a, V b, V c) { return ((c^a)|b)^~a; }
V foo_209_3(V a, V b, V c) { return (~(c|b))|(b&a); }
V foo_209_4(V a, V b, V c) { return (~(c|b))^(b&a); }
V foo_209_5(V a, V b, V c) { return (~((c^a)|b))^a; }
V foo_209_6(V a, V b, V c) { return (~(b&a))^(c|b); }
V foo_209_7(V a, V b, V c) { return ((c^a)|~b)^c; }

V foo_210_1(V a, V b, V c) { return (~b&c)^a; }

V foo_211_1(V a, V b, V c) { return ~((b^a)&(c|b)); }
V foo_211_2(V a, V b, V c) { return (~((c|b)&a))^b; }
V foo_211_3(V a, V b, V c) { return ((c|b)&a)^~b; }

V foo_212_1(V a, V b, V c) { return ((b^a)&(c^a))^b; }
V foo_212_2(V a, V b, V c) { return ((b^a)&(c^b))^a; }
V foo_212_3(V a, V b, V c) { return ((b^a)&c)^(b|a); }
V foo_212_4(V a, V b, V c) { return ((b^a)|(c^a))^c; }

V foo_213_1(V a, V b, V c) { return (b&a)|~c; }

V foo_214_1(V a, V b, V c) { return (((b&a)|c)^a)^b; }
V foo_214_2(V a, V b, V c) { return (((b&a)|c)^b)^a; }
V foo_214_3(V a, V b, V c) { return ((b&a)|c)^(b^a); }
V foo_214_4(V a, V b, V c) { return ((b|a)^c)|(b&a); }
V foo_214_5(V a, V b, V c) { return ((b^a)^c)|(b&a); }

V foo_215_1(V a, V b, V c) { return ~((b^a)&c); }

V foo_216_1(V a, V b, V c) { return ((b^a)&c)^a; }

V foo_217_1(V a, V b, V c) { return ~(((b&a)|c)^b); }
V foo_217_2(V a, V b, V c) { return (~((b&a)|c))^b; }
V foo_217_3(V a, V b, V c) { return ((b&a)|c)^~b; }
V foo_217_4(V a, V b, V c) { return (~(c^b))|(b&a); }

V foo_218_1(V a, V b, V c) { return (c^a)|(c&b); }
V foo_218_2(V a, V b, V c) { return (c^a)|(b&a); }

V foo_219_1(V a, V b, V c) { return ~((b^a)&(c^b)); }
V foo_219_2(V a, V b, V c) { return (~(c^b))|(c^a); }
V foo_219_3(V a, V b, V c) { return (~(b^a))|(c^a); }

V foo_220_1(V a, V b, V c) { return (~c&a)|b; }

V foo_221_1(V a, V b, V c) { return ~c|b; }

V foo_222_1(V a, V b, V c) { return (c^a)|b; }

V foo_223_1(V a, V b, V c) { return (~(c&a))|b; }

V foo_224_1(V a, V b, V c) { return (c|b)&a; }

V foo_225_1(V a, V b, V c) { return ~((c|b)^a); }
V foo_225_2(V a, V b, V c) { return (c|b)^~a; }
V foo_225_3(V a, V b, V c) { return (~(c|b))^a; }

V foo_226_1(V a, V b, V c) { return ((c^a)&b)^c; }

V foo_227_1(V a, V b, V c) { return ~(((c&a)|b)^a); }
V foo_227_2(V a, V b, V c) { return ((c&a)|b)^~a; }
V foo_227_3(V a, V b, V c) { return (~((c&a)|b))^a; }
V foo_227_4(V a, V b, V c) { return (~(b^a))|(c&a); }

V foo_228_1(V a, V b, V c) { return ((b^a)&c)^b; }

V foo_229_1(V a, V b, V c) { return ~(((b&a)|c)^a); }
V foo_229_2(V a, V b, V c) { return ((b&a)|c)^~a; }
V foo_229_3(V a, V b, V c) { return (~((b&a)|c))^a; }
V foo_229_4(V a, V b, V c) { return (~(c^a))|(b&a); }

V foo_230_1(V a, V b, V c) { return (c^b)|(c&a); }
V foo_230_2(V a, V b, V c) { return (c^b)|(b&a); }

V foo_231_1(V a, V b, V c) { return ~((b^a)&(c^a)); }
V foo_231_2(V a, V b, V c) { return (~(c^a))|(c^b); }
V foo_231_3(V a, V b, V c) { return (~(b^a))|(c^b); }

V foo_232_1(V a, V b, V c) { return ((b^a)&(c^a))^a; }
V foo_232_2(V a, V b, V c) { return ((b^a)&(c^b))^b; }
V foo_232_3(V a, V b, V c) { return ((b^a)&c)|(b&a); }
V foo_232_4(V a, V b, V c) { return ((b^a)&c)^(b&a); }
V foo_232_5(V a, V b, V c) { return ((c^a)&(c^b))^c; }
V foo_232_6(V a, V b, V c) { return ((c^a)&b)|(c&a); }
V foo_232_7(V a, V b, V c) { return ((c^a)&b)^(c&a); }
V foo_232_8(V a, V b, V c) { return ((c^b)&a)|(c&b); }
V foo_232_9(V a, V b, V c) { return ((c^b)&a)^(c&b); }
V foo_232_10(V a, V b, V c) { return ((c|b)&a)|(c&b); }
V foo_232_11(V a, V b, V c) { return ((c|a)&b)|(c&a); }
V foo_232_12(V a, V b, V c) { return ((b|a)&c)|(b&a); }
V foo_232_13(V a, V b, V c) { return ((b&a)|c)&(b|a); }
V foo_232_14(V a, V b, V c) { return ((c&a)|b)&(c|a); }
V foo_232_15(V a, V b, V c) { return ((c&b)|a)&(c|b); }

V foo_233_1(V a, V b, V c) { return ~(((b^a)|(c&b))^c); }
V foo_233_2(V a, V b, V c) { return ((b&a)|c)^(~(b|a)); }
V foo_233_3(V a, V b, V c) { return ((c&a)|b)^(~(c|a)); }
V foo_233_4(V a, V b, V c) { return (~((c&b)|a))^(c|b); }
V foo_233_5(V a, V b, V c) { return ((c^b)|(c&a))^~a; }
V foo_233_6(V a, V b, V c) { return ((c&b)|a)^(~(c|b)); }
V foo_233_7(V a, V b, V c) { return (~((c&a)|b))^(c|a); }
V foo_233_8(V a, V b, V c) { return (~((b&a)|c))^(b|a); }
V foo_233_9(V a, V b, V c) { return (~((c^b)|(c&a)))^a; }
V foo_233_10(V a, V b, V c) { return (~((c^a)|(c&b)))^b; }
V foo_233_11(V a, V b, V c) { return ((c^a)|(c&b))^~b; }
V foo_233_12(V a, V b, V c) { return ((b&a)|~c)^(b^a); }
V foo_233_13(V a, V b, V c) { return (~((b^a)|(c&b)))^c; }
V foo_233_14(V a, V b, V c) { return ((b^a)|(c&b))^~c; }
V foo_233_15(V a, V b, V c) { return ((c&a)|~b)^(c^a); }
V foo_233_16(V a, V b, V c) { return ((c&b)|~a)^(c^b); }
V foo_233_17(V a, V b, V c) { return (~((b^a)^c))|(c&b); }
V foo_233_18(V a, V b, V c) { return (~((b^a)^c))|(c&a); }
V foo_233_19(V a, V b, V c) { return (~((b^a)^c))|(b&a); }
V foo_233_20(V a, V b, V c) { return (~((c|b)^a))|(c&b); }
V foo_233_21(V a, V b, V c) { return (~((c|a)^b))|(c&a); }
V foo_233_22(V a, V b, V c) { return (~((b|a)^c))|(b&a); }

V foo_234_1(V a, V b, V c) { return (b&a)|c; }

V foo_235_1(V a, V b, V c) { return (~(b^a))|c; }

V foo_236_1(V a, V b, V c) { return (c&a)|b; }

V foo_237_1(V a, V b, V c) { return (~(c^a))|b; }

V foo_238_1(V a, V b, V c) { return c|b; }

V foo_239_1(V a, V b, V c) { return (c|b)|~a; }
V foo_239_2(V a, V b, V c) { return (~a|b)|c; }
V foo_239_3(V a, V b, V c) { return (~a|c)|b; }

V foo_240_1(V a, V b, V c) { return a; }

V foo_241_1(V a, V b, V c) { return (~(c|b))|a; }

V foo_242_1(V a, V b, V c) { return (~b&c)|a; }

V foo_243_1(V a, V b, V c) { return ~b|a; }

V foo_244_1(V a, V b, V c) { return (~c&b)|a; }

V foo_245_1(V a, V b, V c) { return ~c|a; }

V foo_246_1(V a, V b, V c) { return (c^b)|a; }

V foo_247_1(V a, V b, V c) { return (~(c&b))|a; }

V foo_248_1(V a, V b, V c) { return (c&b)|a; }

V foo_249_1(V a, V b, V c) { return (~(c^b))|a; }

V foo_250_1(V a, V b, V c) { return c|a; }

V foo_251_1(V a, V b, V c) { return (c|a)|~b; }
V foo_251_2(V a, V b, V c) { return (~b|a)|c; }
V foo_251_3(V a, V b, V c) { return (~b|c)|a; }

V foo_252_1(V a, V b, V c) { return b|a; }

V foo_253_1(V a, V b, V c) { return (b|a)|~c; }
V foo_253_2(V a, V b, V c) { return (~c|a)|b; }
V foo_253_3(V a, V b, V c) { return (~c|b)|a; }

V foo_254_1(V a, V b, V c) { return (b|a)|c; }
V foo_254_2(V a, V b, V c) { return (c|a)|b; }
V foo_254_3(V a, V b, V c) { return (c|b)|a; }

V foo_255_1(V a, V b, V c) { return (V){~0,~0,~0,~0}; }

/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]" 673 } } */
