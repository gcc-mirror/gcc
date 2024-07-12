// { dg-do compile { target { x86_64-*-* i?86-*-* }  } }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-fno-vect-cost-model -fno-strict-overflow -fno-delete-null-pointer-checks -fwrapv -fsigned-char -w -mf16c -mavx2 -mfma -mlzcnt -mbmi -mbmi2" }

void *operator new(__SIZE_TYPE__, void *);
struct a {
  a(int, int);
};
typedef float b __attribute__((__vector_size__(16)));
b ad, d, f;
b e(b h, b m) { return __builtin_ia32_maxps(h, m); }
typedef int g __attribute__((__vector_size__(16)));
typedef long long aa __attribute__((__vector_size__(16)));
aa af(b h) { return aa(h); }
aa ae;
struct ah {
  template <typename ag> ah(ag, int);
};
template <typename aj> aj ai(aj h, aj m, aj p) { return an(ak(h, p), m); }
struct q {
  b j;
  q(b h) : j(h) {}
  operator b() { return j; }
};
struct t {
  aa j;
  t(aa h) : j(h) {}
  operator aa() { return j; }
  t(int) : j() {}
  t(b h) : j(af(h)) {}
};
template <int> struct M {
  b j;
  M(b h) : j(h) {}
  operator b() { return j; }
  M(int);
};
template <int ao> struct ap {
  using aq = M<ao>;
  using ar = M<ao>;
  using as = t;
  using at = q;
};
template <int ao> using aq = typename ap<ao>::aq;
template <int ao> using ar = typename ap<ao>::ar;
template <int ao> using as = typename ap<ao>::as;
template <int ao> using at = typename ap<ao>::at;
typedef at<4> au;
ar<4> operator&(ar<4> h, ar<4>) {
  b av, aw = h;
  av = __builtin_ia32_andps(aw, d);
  return av;
}
int ax, ay = 0, az, ba, bb, bc;
bool any(ar<4> h) {
  __attribute__((__vector_size__(4 * sizeof(float)))) float bd = h;
  ax = __builtin_ia32_movmskps(bd);
  return ax;
}
ar<4> operator<(as<4> h, as<4>) {
  aa be, bf = h, ac;
  be = bf < ac;
  return b(be);
}
as<4> ak(as<4> h, as<4>) {
  aa bg, bh = h;
  bg = (aa)__builtin_ia32_pminsd128((g)bh, (g)ae);
  return bg;
}
as<4> an(as<4> h, as<4> m) {
  aa bi, bj = h, bk = m;
  bi = (aa)__builtin_ia32_pmaxsd128((g)bj, (g)bk);
  return bi;
}
au operator-(au h, au) {
  b bl, bm = h;
  bl = bm - ad;
  return bl;
}
au operator*(au h, au m) {
  b bn, bo = h, k = m;
  bn = bo * k;
  return bn;
  __attribute__(()) float bp;
  ;
  ;
}
struct br {
  b bs;
  br() {}
  br(b h) : bs(h) {}
  br(const br &h) { bs = h.bs; }
  operator au() { return bs; }
};
br ak(br &h, br &m) {
  b bq;
  {
    b ab = h.bs;
    b ac = m.bs;
    bq = __builtin_ia32_minps(ab, ac);
  }
  return bq;
}
br an(br h, br m) {
  { f = __builtin_ia32_maxps(h.bs, m.bs); }
  return f;
}
struct bt {
  b bs;
  unsigned n;
  operator br() const { return bs; }
};
template <typename> struct bu {
  br al, am;
  bu() {}
  bu(br h, br m) : al(h), am(m) {}
  void ca(bu &h) {
    al = ak(al, h.al);
    am = an(am, h.am);
  }
  void ca(br h) {
    br bv;
    al = ak(al, h);
    bv = e(am.bs, h.bs);
    am = bv;
  }
  br bw() {
    b bx;
    br by, cf;
    bx = al.bs + am.bs;
    by = bx;
    br cg = by;
    cf = cg;
    return cf;
  }
  br cb() {
    br cc = al.bs + am.bs;
    return cc;
  }
};
typedef bu<br> cd;
template <typename> using ce = ah;
struct ci {
  cd bounds() { return cd(al, am); }
  bt al;
  bt am;
} o;
template <typename bz> struct cm {
  void ch(ci &h) {
    cd bounds = h.bounds();
    cj.ca(bounds);
    br ck = bounds.cb();
    cl.ca(ck);
  }
  bz cj;
  cd cl;
};
template <typename bz> struct cn : cm<bz> {
  void co(ci &h, int m) {
    cm<bz>::ch(h);
    cp += m;
  }
  int cp;
};
struct cq {
  struct cu {
    cu(cq *);
  };
};
template <typename cr, typename cs>
__attribute__((always_inline)) void ct(cn<bu<br>> &h, cn<bu<br>> &m, cr p,
                                       const cs cv) {
  ci *l = &o, *r;
  while (1) {
    while (__builtin_expect(l <= r && p(*l), true)) {
      cv(h, *l);
      ++l;
    }
    while (__builtin_expect(l <= r && p(*r), true)) {
      cv(m, *r);
      --r;
    }
    if (r < l)
      break;
    cv(h, *r);
    cv(m, *l);
  }
}
struct cw {
  as<4> bin(br h) {
    q s = h, cx = s - cy, cz = cx * da;
    as<4> i(cz);
    return ai(i, as<4>(0), as<4>(1));
  }
  au cy, da;
};
struct db {
  cw dc;
};
struct dd : cm<cd> {
  dd();
  dd(cm h) : cm(h) {}
} de;
template <int> struct df {
  typedef db dg;
  typedef int u;
  df(void(), ci *, cm<cd>);
  u dh;
  void di(u, dd, dd, dd) { dj(); }
  dg &v;
  dd w;
  dd x;
  a dj() {
    cn<cd> y, z;
    int dk;
    as<4> dl(0);
    aq<4> dm(dk);
    ct(
        y, z,
        [&](ci &dn) {
          br __trans_tmp_1293 = dn.bounds().bw(),
             __trans_tmp_1292 = __trans_tmp_1293,
             __trans_tmp_1291 = __trans_tmp_1292,
             __trans_tmp_1290 = __trans_tmp_1291,
             __trans_tmp_1289 = __trans_tmp_1290,
             __trans_tmp_1288 = __trans_tmp_1289,
             __trans_tmp_1287 = __trans_tmp_1288,
             __trans_tmp_1286 = __trans_tmp_1287,
             __trans_tmp_1285 = __trans_tmp_1286,
             __trans_tmp_1284 = __trans_tmp_1285,
             __trans_tmp_1283 = __trans_tmp_1284,
             __trans_tmp_1282 = __trans_tmp_1283,
             __trans_tmp_1281 = __trans_tmp_1282,
             __trans_tmp_1280 = __trans_tmp_1281,
             __trans_tmp_1279 = __trans_tmp_1280,
             __trans_tmp_1278 = __trans_tmp_1279,
             __trans_tmp_1277 = __trans_tmp_1278,
             __trans_tmp_1276 = __trans_tmp_1277,
             __trans_tmp_1275 = __trans_tmp_1276,
             __trans_tmp_1274 = __trans_tmp_1275,
             __trans_tmp_1273 = __trans_tmp_1274,
             __trans_tmp_1272 = __trans_tmp_1273,
             __trans_tmp_1271 = __trans_tmp_1272,
             __trans_tmp_1270 = __trans_tmp_1271,
             __trans_tmp_1269 = __trans_tmp_1270,
             __trans_tmp_1268 = __trans_tmp_1269,
             __trans_tmp_1267 = __trans_tmp_1268,
             __trans_tmp_1266 = __trans_tmp_1267,
             __trans_tmp_1265 = __trans_tmp_1266,
             __trans_tmp_1264 = __trans_tmp_1265,
             __trans_tmp_1263 = __trans_tmp_1264,
             __trans_tmp_1262 = __trans_tmp_1263,
             __trans_tmp_1261 = __trans_tmp_1262,
             __trans_tmp_1260 = __trans_tmp_1261,
             __trans_tmp_1259 = __trans_tmp_1260,
             __trans_tmp_1258 = __trans_tmp_1259,
             __trans_tmp_1257 = __trans_tmp_1258,
             __trans_tmp_1256 = __trans_tmp_1257,
             __trans_tmp_1255 = __trans_tmp_1256,
             __trans_tmp_1254 = __trans_tmp_1255,
             __trans_tmp_1253 = __trans_tmp_1254,
             __trans_tmp_1252 = __trans_tmp_1253,
             __trans_tmp_1251 = __trans_tmp_1252,
             __trans_tmp_1250 = __trans_tmp_1251,
             __trans_tmp_1249 = __trans_tmp_1250,
             __trans_tmp_1248 = __trans_tmp_1249,
             __trans_tmp_1247 = __trans_tmp_1248,
             __trans_tmp_1246 = __trans_tmp_1247,
             __trans_tmp_1245 = __trans_tmp_1246,
             __trans_tmp_1244 = __trans_tmp_1245,
             __trans_tmp_1243 = __trans_tmp_1244,
             __trans_tmp_1242 = __trans_tmp_1243,
             __trans_tmp_1241 = __trans_tmp_1242,
             __trans_tmp_1240 = __trans_tmp_1241,
             __trans_tmp_1239 = __trans_tmp_1240,
             __trans_tmp_1238 = __trans_tmp_1239,
             __trans_tmp_1237 = __trans_tmp_1238,
             __trans_tmp_1236 = __trans_tmp_1237,
             __trans_tmp_1235 = __trans_tmp_1236,
             __trans_tmp_1234 = __trans_tmp_1235,
             __trans_tmp_1233 = __trans_tmp_1234,
             __trans_tmp_1232 = __trans_tmp_1233,
             __trans_tmp_1231 = __trans_tmp_1232,
             __trans_tmp_1230 = __trans_tmp_1231,
             __trans_tmp_1229 = __trans_tmp_1230,
             __trans_tmp_1228 = __trans_tmp_1229,
             __trans_tmp_1227 = __trans_tmp_1228,
             __trans_tmp_1226 = __trans_tmp_1227,
             __trans_tmp_1225 = __trans_tmp_1226,
             __trans_tmp_1224 = __trans_tmp_1225,
             __trans_tmp_1223 = __trans_tmp_1224,
             __trans_tmp_1222 = __trans_tmp_1223,
             __trans_tmp_1221 = __trans_tmp_1222,
             __trans_tmp_1220 = __trans_tmp_1221,
             __trans_tmp_1219 = __trans_tmp_1220,
             __trans_tmp_1218 = __trans_tmp_1219,
             __trans_tmp_1217 = __trans_tmp_1218,
             __trans_tmp_1216 = __trans_tmp_1217,
             __trans_tmp_1215 = __trans_tmp_1216,
             __trans_tmp_1214 = __trans_tmp_1215,
             __trans_tmp_1213 = __trans_tmp_1214,
             __trans_tmp_1212 = __trans_tmp_1213,
             __trans_tmp_1211 = __trans_tmp_1212,
             __trans_tmp_1210 = __trans_tmp_1211,
             __trans_tmp_1209 = __trans_tmp_1210,
             __trans_tmp_1208 = __trans_tmp_1209,
             __trans_tmp_1207 = __trans_tmp_1208,
             __trans_tmp_1206 = __trans_tmp_1207,
             __trans_tmp_1205 = __trans_tmp_1206,
             __trans_tmp_1204 = __trans_tmp_1205,
             __trans_tmp_1203 = __trans_tmp_1204,
             __trans_tmp_1202 = __trans_tmp_1203,
             __trans_tmp_1201 = __trans_tmp_1202,
             __trans_tmp_1200 = __trans_tmp_1201,
             __trans_tmp_1199 = __trans_tmp_1200,
             __trans_tmp_1198 = __trans_tmp_1199,
             __trans_tmp_1197 = __trans_tmp_1198,
             __trans_tmp_1196 = __trans_tmp_1197,
             __trans_tmp_1195 = __trans_tmp_1196,
             __trans_tmp_1194 = __trans_tmp_1195,
             __trans_tmp_1193 = __trans_tmp_1194,
             __trans_tmp_1192 = __trans_tmp_1193,
             __trans_tmp_1191 = __trans_tmp_1192,
             __trans_tmp_1190 = __trans_tmp_1191,
             __trans_tmp_1189 = __trans_tmp_1190,
             __trans_tmp_1188 = __trans_tmp_1189,
             __trans_tmp_1187 = __trans_tmp_1188,
             __trans_tmp_1186 = __trans_tmp_1187,
             __trans_tmp_1185 = __trans_tmp_1186,
             __trans_tmp_1184 = __trans_tmp_1185,
             __trans_tmp_1183 = __trans_tmp_1184,
             __trans_tmp_1182 = __trans_tmp_1183,
             __trans_tmp_1181 = __trans_tmp_1182,
             __trans_tmp_1180 = __trans_tmp_1181,
             __trans_tmp_1179 = __trans_tmp_1180,
             __trans_tmp_1178 = __trans_tmp_1179,
             __trans_tmp_1177 = __trans_tmp_1178,
             __trans_tmp_1176 = __trans_tmp_1177,
             __trans_tmp_1175 = __trans_tmp_1176,
             __trans_tmp_1174 = __trans_tmp_1175,
             __trans_tmp_1173 = __trans_tmp_1174,
             __trans_tmp_1172 = __trans_tmp_1173,
             __trans_tmp_1171 = __trans_tmp_1172,
             __trans_tmp_1170 = __trans_tmp_1171,
             __trans_tmp_1169 = __trans_tmp_1170,
             __trans_tmp_1168 = __trans_tmp_1169,
             __trans_tmp_1167 = __trans_tmp_1168,
             __trans_tmp_1166 = __trans_tmp_1167,
             __trans_tmp_1165 = __trans_tmp_1166,
             __trans_tmp_1164 = __trans_tmp_1165,
             __trans_tmp_1163 = __trans_tmp_1164,
             __trans_tmp_1162 = __trans_tmp_1163,
             __trans_tmp_1161 = __trans_tmp_1162,
             __trans_tmp_1160 = __trans_tmp_1161,
             __trans_tmp_1159 = __trans_tmp_1160,
             __trans_tmp_1158 = __trans_tmp_1159,
             __trans_tmp_1157 = __trans_tmp_1158,
             __trans_tmp_1156 = __trans_tmp_1157,
             __trans_tmp_1155 = __trans_tmp_1156,
             __trans_tmp_1154 = __trans_tmp_1155,
             __trans_tmp_1153 = __trans_tmp_1154,
             __trans_tmp_1152 = __trans_tmp_1153,
             __trans_tmp_1151 = __trans_tmp_1152,
             __trans_tmp_1150 = __trans_tmp_1151,
             __trans_tmp_1149 = __trans_tmp_1150,
             __trans_tmp_1148 = __trans_tmp_1149,
             __trans_tmp_1147 = __trans_tmp_1148,
             __trans_tmp_1146 = __trans_tmp_1147,
             __trans_tmp_1145 = __trans_tmp_1146,
             __trans_tmp_1144 = __trans_tmp_1145,
             __trans_tmp_1143 = __trans_tmp_1144,
             __trans_tmp_1142 = __trans_tmp_1143,
             __trans_tmp_1141 = __trans_tmp_1142,
             __trans_tmp_1140 = __trans_tmp_1141,
             __trans_tmp_1139 = __trans_tmp_1140,
             __trans_tmp_1138 = __trans_tmp_1139,
             __trans_tmp_1137 = __trans_tmp_1138,
             __trans_tmp_1136 = __trans_tmp_1137,
             __trans_tmp_1135 = __trans_tmp_1136,
             __trans_tmp_1134 = __trans_tmp_1135,
             __trans_tmp_1133 = __trans_tmp_1134,
             __trans_tmp_1132 = __trans_tmp_1133,
             __trans_tmp_1131 = __trans_tmp_1132,
             __trans_tmp_1130 = __trans_tmp_1131,
             __trans_tmp_1129 = __trans_tmp_1130,
             __trans_tmp_1128 = __trans_tmp_1129,
             __trans_tmp_1127 = __trans_tmp_1128,
             __trans_tmp_1126 = __trans_tmp_1127,
             __trans_tmp_1125 = __trans_tmp_1126,
             __trans_tmp_1124 = __trans_tmp_1125,
             __trans_tmp_1123 = __trans_tmp_1124,
             __trans_tmp_1122 = __trans_tmp_1123,
             __trans_tmp_1121 = __trans_tmp_1122,
             __trans_tmp_1120 = __trans_tmp_1121,
             __trans_tmp_1119 = __trans_tmp_1120,
             __trans_tmp_1118 = __trans_tmp_1119,
             __trans_tmp_1117 = __trans_tmp_1118,
             __trans_tmp_1116 = __trans_tmp_1117,
             __trans_tmp_1115 = __trans_tmp_1116,
             __trans_tmp_1114 = __trans_tmp_1115,
             __trans_tmp_1113 = __trans_tmp_1114,
             __trans_tmp_1112 = __trans_tmp_1113,
             __trans_tmp_1111 = __trans_tmp_1112,
             __trans_tmp_1110 = __trans_tmp_1111,
             __trans_tmp_1109 = __trans_tmp_1110,
             __trans_tmp_1108 = __trans_tmp_1109,
             __trans_tmp_1107 = __trans_tmp_1108,
             __trans_tmp_1106 = __trans_tmp_1107,
             __trans_tmp_1105 = __trans_tmp_1106,
             __trans_tmp_1104 = __trans_tmp_1105,
             __trans_tmp_1103 = __trans_tmp_1104,
             __trans_tmp_1102 = __trans_tmp_1103,
             __trans_tmp_1101 = __trans_tmp_1102,
             __trans_tmp_1100 = __trans_tmp_1101,
             __trans_tmp_1099 = __trans_tmp_1100,
             __trans_tmp_1098 = __trans_tmp_1099,
             __trans_tmp_1097 = __trans_tmp_1098,
             __trans_tmp_1096 = __trans_tmp_1097,
             __trans_tmp_1095 = __trans_tmp_1096,
             __trans_tmp_1094 = __trans_tmp_1095,
             __trans_tmp_1093 = __trans_tmp_1094,
             __trans_tmp_1092 = __trans_tmp_1093,
             __trans_tmp_1091 = __trans_tmp_1092,
             __trans_tmp_1090 = __trans_tmp_1091,
             __trans_tmp_1089 = __trans_tmp_1090,
             __trans_tmp_1088 = __trans_tmp_1089,
             __trans_tmp_1087 = __trans_tmp_1088,
             __trans_tmp_1086 = __trans_tmp_1087,
             __trans_tmp_1085 = __trans_tmp_1086,
             __trans_tmp_1084 = __trans_tmp_1085,
             __trans_tmp_1083 = __trans_tmp_1084,
             __trans_tmp_1082 = __trans_tmp_1083,
             __trans_tmp_1081 = __trans_tmp_1082,
             __trans_tmp_1080 = __trans_tmp_1081,
             __trans_tmp_1079 = __trans_tmp_1080,
             __trans_tmp_1078 = __trans_tmp_1079,
             __trans_tmp_1077 = __trans_tmp_1078,
             __trans_tmp_1076 = __trans_tmp_1077,
             __trans_tmp_1075 = __trans_tmp_1076,
             __trans_tmp_1074 = __trans_tmp_1075,
             __trans_tmp_1073 = __trans_tmp_1074,
             __trans_tmp_1072 = __trans_tmp_1073,
             __trans_tmp_1071 = __trans_tmp_1072,
             __trans_tmp_1070 = __trans_tmp_1071,
             __trans_tmp_1069 = __trans_tmp_1070,
             __trans_tmp_1068 = __trans_tmp_1069,
             __trans_tmp_1067 = __trans_tmp_1068,
             __trans_tmp_1066 = __trans_tmp_1067,
             __trans_tmp_1065 = __trans_tmp_1066,
             __trans_tmp_1064 = __trans_tmp_1065,
             __trans_tmp_1063 = __trans_tmp_1064,
             __trans_tmp_1062 = __trans_tmp_1063,
             __trans_tmp_1061 = __trans_tmp_1062,
             __trans_tmp_1060 = __trans_tmp_1061,
             __trans_tmp_1059 = __trans_tmp_1060,
             __trans_tmp_1058 = __trans_tmp_1059,
             __trans_tmp_1057 = __trans_tmp_1058,
             __trans_tmp_1056 = __trans_tmp_1057,
             __trans_tmp_1055 = __trans_tmp_1056,
             __trans_tmp_1054 = __trans_tmp_1055,
             __trans_tmp_1053 = __trans_tmp_1054,
             __trans_tmp_1052 = __trans_tmp_1053,
             __trans_tmp_1051 = __trans_tmp_1052,
             __trans_tmp_1050 = __trans_tmp_1051,
             __trans_tmp_1049 = __trans_tmp_1050,
             __trans_tmp_1048 = __trans_tmp_1049,
             __trans_tmp_1047 = __trans_tmp_1048,
             __trans_tmp_1046 = __trans_tmp_1047,
             __trans_tmp_1045 = __trans_tmp_1046,
             __trans_tmp_1044 = __trans_tmp_1045,
             __trans_tmp_1043 = __trans_tmp_1044,
             __trans_tmp_1042 = __trans_tmp_1043,
             __trans_tmp_1041 = __trans_tmp_1042,
             __trans_tmp_1040 = __trans_tmp_1041,
             __trans_tmp_1039 = __trans_tmp_1040,
             __trans_tmp_1038 = __trans_tmp_1039,
             __trans_tmp_1037 = __trans_tmp_1038,
             __trans_tmp_1036 = __trans_tmp_1037,
             __trans_tmp_1035 = __trans_tmp_1036,
             __trans_tmp_1034 = __trans_tmp_1035,
             __trans_tmp_1033 = __trans_tmp_1034,
             __trans_tmp_1032 = __trans_tmp_1033,
             __trans_tmp_1031 = __trans_tmp_1032,
             __trans_tmp_1030 = __trans_tmp_1031,
             __trans_tmp_1029 = __trans_tmp_1030,
             __trans_tmp_1028 = __trans_tmp_1029,
             __trans_tmp_1027 = __trans_tmp_1028,
             __trans_tmp_1026 = __trans_tmp_1027,
             __trans_tmp_1025 = __trans_tmp_1026,
             __trans_tmp_1024 = __trans_tmp_1025,
             __trans_tmp_1023 = __trans_tmp_1024,
             __trans_tmp_1022 = __trans_tmp_1023,
             __trans_tmp_1021 = __trans_tmp_1022,
             __trans_tmp_1020 = __trans_tmp_1021,
             __trans_tmp_1019 = __trans_tmp_1020,
             __trans_tmp_1018 = __trans_tmp_1019,
             __trans_tmp_1017 = __trans_tmp_1018,
             __trans_tmp_1016 = __trans_tmp_1017,
             __trans_tmp_1015 = __trans_tmp_1016,
             __trans_tmp_1014 = __trans_tmp_1015,
             __trans_tmp_1013 = __trans_tmp_1014,
             __trans_tmp_1012 = __trans_tmp_1013,
             __trans_tmp_1011 = __trans_tmp_1012,
             __trans_tmp_1010 = __trans_tmp_1011,
             __trans_tmp_1009 = __trans_tmp_1010,
             __trans_tmp_1008 = __trans_tmp_1009,
             __trans_tmp_1007 = __trans_tmp_1008,
             __trans_tmp_1006 = __trans_tmp_1007,
             __trans_tmp_1005 = __trans_tmp_1006,
             __trans_tmp_1004 = __trans_tmp_1005,
             __trans_tmp_1003 = __trans_tmp_1004,
             __trans_tmp_1002 = __trans_tmp_1003,
             __trans_tmp_1001 = __trans_tmp_1002,
             __trans_tmp_1000 = __trans_tmp_1001,
             __trans_tmp_999 = __trans_tmp_1000,
             __trans_tmp_998 = __trans_tmp_999,
             __trans_tmp_997 = __trans_tmp_998,
             __trans_tmp_996 = __trans_tmp_997,
             __trans_tmp_995 = __trans_tmp_996,
             __trans_tmp_994 = __trans_tmp_995,
             __trans_tmp_993 = __trans_tmp_994,
             __trans_tmp_992 = __trans_tmp_993,
             __trans_tmp_991 = __trans_tmp_992,
             __trans_tmp_990 = __trans_tmp_991,
             __trans_tmp_989 = __trans_tmp_990,
             __trans_tmp_988 = __trans_tmp_989,
             __trans_tmp_987 = __trans_tmp_988,
             __trans_tmp_986 = __trans_tmp_987,
             __trans_tmp_985 = __trans_tmp_986,
             __trans_tmp_984 = __trans_tmp_985,
             __trans_tmp_983 = __trans_tmp_984,
             __trans_tmp_982 = __trans_tmp_983,
             __trans_tmp_981 = __trans_tmp_982,
             __trans_tmp_980 = __trans_tmp_981,
             __trans_tmp_979 = __trans_tmp_980,
             __trans_tmp_978 = __trans_tmp_979,
             __trans_tmp_977 = __trans_tmp_978,
             __trans_tmp_976 = __trans_tmp_977,
             __trans_tmp_975 = __trans_tmp_976,
             __trans_tmp_974 = __trans_tmp_975,
             __trans_tmp_973 = __trans_tmp_974,
             __trans_tmp_972 = __trans_tmp_973,
             __trans_tmp_971 = __trans_tmp_972,
             __trans_tmp_970 = __trans_tmp_971,
             __trans_tmp_969 = __trans_tmp_970,
             __trans_tmp_968 = __trans_tmp_969,
             __trans_tmp_967 = __trans_tmp_968,
             __trans_tmp_966 = __trans_tmp_967,
             __trans_tmp_965 = __trans_tmp_966,
             __trans_tmp_964 = __trans_tmp_965,
             __trans_tmp_963 = __trans_tmp_964,
             __trans_tmp_962 = __trans_tmp_963,
             __trans_tmp_961 = __trans_tmp_962,
             __trans_tmp_960 = __trans_tmp_961,
             __trans_tmp_959 = __trans_tmp_960,
             __trans_tmp_958 = __trans_tmp_959,
             __trans_tmp_957 = __trans_tmp_958,
             __trans_tmp_956 = __trans_tmp_957,
             __trans_tmp_955 = __trans_tmp_956,
             __trans_tmp_954 = __trans_tmp_955,
             __trans_tmp_953 = __trans_tmp_954,
             __trans_tmp_952 = __trans_tmp_953,
             __trans_tmp_951 = __trans_tmp_952,
             __trans_tmp_950 = __trans_tmp_951,
             __trans_tmp_949 = __trans_tmp_950,
             __trans_tmp_948 = __trans_tmp_949,
             __trans_tmp_947 = __trans_tmp_948,
             __trans_tmp_946 = __trans_tmp_947,
             __trans_tmp_945 = __trans_tmp_946,
             __trans_tmp_944 = __trans_tmp_945,
             __trans_tmp_943 = __trans_tmp_944,
             __trans_tmp_942 = __trans_tmp_943,
             __trans_tmp_941 = __trans_tmp_942,
             __trans_tmp_940 = __trans_tmp_941,
             __trans_tmp_939 = __trans_tmp_940,
             __trans_tmp_938 = __trans_tmp_939,
             __trans_tmp_937 = __trans_tmp_938,
             __trans_tmp_936 = __trans_tmp_937,
             __trans_tmp_935 = __trans_tmp_936,
             __trans_tmp_934 = __trans_tmp_935,
             __trans_tmp_933 = __trans_tmp_934,
             __trans_tmp_932 = __trans_tmp_933,
             __trans_tmp_931 = __trans_tmp_932,
             __trans_tmp_930 = __trans_tmp_931,
             __trans_tmp_929 = __trans_tmp_930,
             __trans_tmp_928 = __trans_tmp_929,
             __trans_tmp_927 = __trans_tmp_928,
             __trans_tmp_926 = __trans_tmp_927,
             __trans_tmp_925 = __trans_tmp_926,
             __trans_tmp_924 = __trans_tmp_925,
             __trans_tmp_923 = __trans_tmp_924,
             __trans_tmp_922 = __trans_tmp_923,
             __trans_tmp_921 = __trans_tmp_922,
             __trans_tmp_920 = __trans_tmp_921,
             __trans_tmp_919 = __trans_tmp_920,
             __trans_tmp_918 = __trans_tmp_919,
             __trans_tmp_917 = __trans_tmp_918,
             __trans_tmp_916 = __trans_tmp_917,
             __trans_tmp_915 = __trans_tmp_916,
             __trans_tmp_914 = __trans_tmp_915,
             __trans_tmp_913 = __trans_tmp_914,
             __trans_tmp_912 = __trans_tmp_913,
             __trans_tmp_911 = __trans_tmp_912,
             __trans_tmp_910 = __trans_tmp_911,
             __trans_tmp_909 = __trans_tmp_910,
             __trans_tmp_908 = __trans_tmp_909,
             __trans_tmp_907 = __trans_tmp_908,
             __trans_tmp_906 = __trans_tmp_907,
             __trans_tmp_905 = __trans_tmp_906,
             __trans_tmp_904 = __trans_tmp_905,
             __trans_tmp_903 = __trans_tmp_904,
             __trans_tmp_902 = __trans_tmp_903,
             __trans_tmp_901 = __trans_tmp_902,
             __trans_tmp_900 = __trans_tmp_901,
             __trans_tmp_899 = __trans_tmp_900,
             __trans_tmp_898 = __trans_tmp_899,
             __trans_tmp_897 = __trans_tmp_898,
             __trans_tmp_896 = __trans_tmp_897,
             __trans_tmp_895 = __trans_tmp_896,
             __trans_tmp_894 = __trans_tmp_895,
             __trans_tmp_893 = __trans_tmp_894,
             __trans_tmp_892 = __trans_tmp_893,
             __trans_tmp_891 = __trans_tmp_892,
             __trans_tmp_890 = __trans_tmp_891,
             __trans_tmp_889 = __trans_tmp_890,
             __trans_tmp_888 = __trans_tmp_889,
             __trans_tmp_887 = __trans_tmp_888,
             __trans_tmp_886 = __trans_tmp_887,
             __trans_tmp_885 = __trans_tmp_886,
             __trans_tmp_884 = __trans_tmp_885,
             __trans_tmp_883 = __trans_tmp_884,
             __trans_tmp_882 = __trans_tmp_883,
             __trans_tmp_881 = __trans_tmp_882,
             __trans_tmp_880 = __trans_tmp_881,
             __trans_tmp_879 = __trans_tmp_880,
             __trans_tmp_878 = __trans_tmp_879,
             __trans_tmp_877 = __trans_tmp_878,
             __trans_tmp_876 = __trans_tmp_877,
             __trans_tmp_875 = __trans_tmp_876,
             __trans_tmp_874 = __trans_tmp_875,
             __trans_tmp_873 = __trans_tmp_874,
             __trans_tmp_872 = __trans_tmp_873,
             __trans_tmp_871 = __trans_tmp_872,
             __trans_tmp_870 = __trans_tmp_871,
             __trans_tmp_869 = __trans_tmp_870,
             __trans_tmp_868 = __trans_tmp_869,
             __trans_tmp_867 = __trans_tmp_868,
             __trans_tmp_866 = __trans_tmp_867,
             __trans_tmp_865 = __trans_tmp_866,
             __trans_tmp_864 = __trans_tmp_865,
             __trans_tmp_863 = __trans_tmp_864, c = __trans_tmp_863;
          bool __trans_tmp_861 = any(v.dc.bin(c) < dl & dm);
          return __trans_tmp_861;
        },
        [](cn<cd> &pinfo, ci &dn) { pinfo.co(dn, dn.al.n); });
    int left_weight = y.cp;
    new (&w) dd(y);
    new (&x) dd(z);
    a(left_weight, z.cp);
  }
};
struct G {};
struct J {
  J(int, df<16>, cq::cu, int, int, cq *, int, int, int, int);
  void recurse(G h, cq::cu, bool) { heuristic.di(ay, de, de, de); }
  df<6> heuristic;
};
struct I {
  template <typename, typename Heuristic, typename, typename>
  static void build(Heuristic h, cq::cu m, cq *p) {
    J builder(0, h, m, bc, bb, p, int(), int(), ba, az);
    G record;
    builder.recurse(record, nullptr, true);
  }
};
struct BVHBuilderBinnedFastSpatialSAH {
  template <typename SplitPrimitiveFunc>
  static void build(cq::cu h, int, int, cq *cv, SplitPrimitiveFunc p5, int,
                    cn<cd> p7, int) {
    ci de;
    typedef df<16> Heuristic;
    Heuristic heuristic(p5, &de, p7);
    I::build<int, Heuristic, dd, ci>(heuristic, h, cv);
  }
} BVHNBuilderFastSpatialSAH_scene;
struct BVHNBuilderFastSpatialSAH {
  cq bvh;
  ce<ci> prims0;
  int settings;
  BVHNBuilderFastSpatialSAH() : prims0(BVHNBuilderFastSpatialSAH_scene, 0) {
    cn<cd> pinfo;
    void splitter();
    BVHBuilderBinnedFastSpatialSAH::build(&bvh, int(), int(), &bvh, splitter, 0,
                                          pinfo, settings);
  }
} BVH4Triangle4SceneBuilderFastSpatialSAH_bvh;
