/* { dg-do run { target { power10_hw } } } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#include <altivec.h>

typedef unsigned char uint8_t;

template <uint8_t kTernLogOp>
static inline vector unsigned long long
VSXTernaryLogic (vector unsigned long long a, vector unsigned long long b,
		 vector unsigned long long c)
{
  return vec_ternarylogic (a, b, c, kTernLogOp);
}

static vector unsigned long long
VSXTernaryLogic (vector unsigned long long a, vector unsigned long long b,
		 vector unsigned long long c, int ternary_logic_op)
{
  switch (ternary_logic_op & 0xFF)
    {
    case 0:
      return VSXTernaryLogic<0> (a, b, c);
    case 1:
      return VSXTernaryLogic<1> (a, b, c);
    case 2:
      return VSXTernaryLogic<2> (a, b, c);
    case 3:
      return VSXTernaryLogic<3> (a, b, c);
    case 4:
      return VSXTernaryLogic<4> (a, b, c);
    case 5:
      return VSXTernaryLogic<5> (a, b, c);
    case 6:
      return VSXTernaryLogic<6> (a, b, c);
    case 7:
      return VSXTernaryLogic<7> (a, b, c);
    case 8:
      return VSXTernaryLogic<8> (a, b, c);
    case 9:
      return VSXTernaryLogic<9> (a, b, c);
    case 10:
      return VSXTernaryLogic<10> (a, b, c);
    case 11:
      return VSXTernaryLogic<11> (a, b, c);
    case 12:
      return VSXTernaryLogic<12> (a, b, c);
    case 13:
      return VSXTernaryLogic<13> (a, b, c);
    case 14:
      return VSXTernaryLogic<14> (a, b, c);
    case 15:
      return VSXTernaryLogic<15> (a, b, c);
    case 16:
      return VSXTernaryLogic<16> (a, b, c);
    case 17:
      return VSXTernaryLogic<17> (a, b, c);
    case 18:
      return VSXTernaryLogic<18> (a, b, c);
    case 19:
      return VSXTernaryLogic<19> (a, b, c);
    case 20:
      return VSXTernaryLogic<20> (a, b, c);
    case 21:
      return VSXTernaryLogic<21> (a, b, c);
    case 22:
      return VSXTernaryLogic<22> (a, b, c);
    case 23:
      return VSXTernaryLogic<23> (a, b, c);
    case 24:
      return VSXTernaryLogic<24> (a, b, c);
    case 25:
      return VSXTernaryLogic<25> (a, b, c);
    case 26:
      return VSXTernaryLogic<26> (a, b, c);
    case 27:
      return VSXTernaryLogic<27> (a, b, c);
    case 28:
      return VSXTernaryLogic<28> (a, b, c);
    case 29:
      return VSXTernaryLogic<29> (a, b, c);
    case 30:
      return VSXTernaryLogic<30> (a, b, c);
    case 31:
      return VSXTernaryLogic<31> (a, b, c);
    case 32:
      return VSXTernaryLogic<32> (a, b, c);
    case 33:
      return VSXTernaryLogic<33> (a, b, c);
    case 34:
      return VSXTernaryLogic<34> (a, b, c);
    case 35:
      return VSXTernaryLogic<35> (a, b, c);
    case 36:
      return VSXTernaryLogic<36> (a, b, c);
    case 37:
      return VSXTernaryLogic<37> (a, b, c);
    case 38:
      return VSXTernaryLogic<38> (a, b, c);
    case 39:
      return VSXTernaryLogic<39> (a, b, c);
    case 40:
      return VSXTernaryLogic<40> (a, b, c);
    case 41:
      return VSXTernaryLogic<41> (a, b, c);
    case 42:
      return VSXTernaryLogic<42> (a, b, c);
    case 43:
      return VSXTernaryLogic<43> (a, b, c);
    case 44:
      return VSXTernaryLogic<44> (a, b, c);
    case 45:
      return VSXTernaryLogic<45> (a, b, c);
    case 46:
      return VSXTernaryLogic<46> (a, b, c);
    case 47:
      return VSXTernaryLogic<47> (a, b, c);
    case 48:
      return VSXTernaryLogic<48> (a, b, c);
    case 49:
      return VSXTernaryLogic<49> (a, b, c);
    case 50:
      return VSXTernaryLogic<50> (a, b, c);
    case 51:
      return VSXTernaryLogic<51> (a, b, c);
    case 52:
      return VSXTernaryLogic<52> (a, b, c);
    case 53:
      return VSXTernaryLogic<53> (a, b, c);
    case 54:
      return VSXTernaryLogic<54> (a, b, c);
    case 55:
      return VSXTernaryLogic<55> (a, b, c);
    case 56:
      return VSXTernaryLogic<56> (a, b, c);
    case 57:
      return VSXTernaryLogic<57> (a, b, c);
    case 58:
      return VSXTernaryLogic<58> (a, b, c);
    case 59:
      return VSXTernaryLogic<59> (a, b, c);
    case 60:
      return VSXTernaryLogic<60> (a, b, c);
    case 61:
      return VSXTernaryLogic<61> (a, b, c);
    case 62:
      return VSXTernaryLogic<62> (a, b, c);
    case 63:
      return VSXTernaryLogic<63> (a, b, c);
    case 64:
      return VSXTernaryLogic<64> (a, b, c);
    case 65:
      return VSXTernaryLogic<65> (a, b, c);
    case 66:
      return VSXTernaryLogic<66> (a, b, c);
    case 67:
      return VSXTernaryLogic<67> (a, b, c);
    case 68:
      return VSXTernaryLogic<68> (a, b, c);
    case 69:
      return VSXTernaryLogic<69> (a, b, c);
    case 70:
      return VSXTernaryLogic<70> (a, b, c);
    case 71:
      return VSXTernaryLogic<71> (a, b, c);
    case 72:
      return VSXTernaryLogic<72> (a, b, c);
    case 73:
      return VSXTernaryLogic<73> (a, b, c);
    case 74:
      return VSXTernaryLogic<74> (a, b, c);
    case 75:
      return VSXTernaryLogic<75> (a, b, c);
    case 76:
      return VSXTernaryLogic<76> (a, b, c);
    case 77:
      return VSXTernaryLogic<77> (a, b, c);
    case 78:
      return VSXTernaryLogic<78> (a, b, c);
    case 79:
      return VSXTernaryLogic<79> (a, b, c);
    case 80:
      return VSXTernaryLogic<80> (a, b, c);
    case 81:
      return VSXTernaryLogic<81> (a, b, c);
    case 82:
      return VSXTernaryLogic<82> (a, b, c);
    case 83:
      return VSXTernaryLogic<83> (a, b, c);
    case 84:
      return VSXTernaryLogic<84> (a, b, c);
    case 85:
      return VSXTernaryLogic<85> (a, b, c);
    case 86:
      return VSXTernaryLogic<86> (a, b, c);
    case 87:
      return VSXTernaryLogic<87> (a, b, c);
    case 88:
      return VSXTernaryLogic<88> (a, b, c);
    case 89:
      return VSXTernaryLogic<89> (a, b, c);
    case 90:
      return VSXTernaryLogic<90> (a, b, c);
    case 91:
      return VSXTernaryLogic<91> (a, b, c);
    case 92:
      return VSXTernaryLogic<92> (a, b, c);
    case 93:
      return VSXTernaryLogic<93> (a, b, c);
    case 94:
      return VSXTernaryLogic<94> (a, b, c);
    case 95:
      return VSXTernaryLogic<95> (a, b, c);
    case 96:
      return VSXTernaryLogic<96> (a, b, c);
    case 97:
      return VSXTernaryLogic<97> (a, b, c);
    case 98:
      return VSXTernaryLogic<98> (a, b, c);
    case 99:
      return VSXTernaryLogic<99> (a, b, c);
    case 100:
      return VSXTernaryLogic<100> (a, b, c);
    case 101:
      return VSXTernaryLogic<101> (a, b, c);
    case 102:
      return VSXTernaryLogic<102> (a, b, c);
    case 103:
      return VSXTernaryLogic<103> (a, b, c);
    case 104:
      return VSXTernaryLogic<104> (a, b, c);
    case 105:
      return VSXTernaryLogic<105> (a, b, c);
    case 106:
      return VSXTernaryLogic<106> (a, b, c);
    case 107:
      return VSXTernaryLogic<107> (a, b, c);
    case 108:
      return VSXTernaryLogic<108> (a, b, c);
    case 109:
      return VSXTernaryLogic<109> (a, b, c);
    case 110:
      return VSXTernaryLogic<110> (a, b, c);
    case 111:
      return VSXTernaryLogic<111> (a, b, c);
    case 112:
      return VSXTernaryLogic<112> (a, b, c);
    case 113:
      return VSXTernaryLogic<113> (a, b, c);
    case 114:
      return VSXTernaryLogic<114> (a, b, c);
    case 115:
      return VSXTernaryLogic<115> (a, b, c);
    case 116:
      return VSXTernaryLogic<116> (a, b, c);
    case 117:
      return VSXTernaryLogic<117> (a, b, c);
    case 118:
      return VSXTernaryLogic<118> (a, b, c);
    case 119:
      return VSXTernaryLogic<119> (a, b, c);
    case 120:
      return VSXTernaryLogic<120> (a, b, c);
    case 121:
      return VSXTernaryLogic<121> (a, b, c);
    case 122:
      return VSXTernaryLogic<122> (a, b, c);
    case 123:
      return VSXTernaryLogic<123> (a, b, c);
    case 124:
      return VSXTernaryLogic<124> (a, b, c);
    case 125:
      return VSXTernaryLogic<125> (a, b, c);
    case 126:
      return VSXTernaryLogic<126> (a, b, c);
    case 127:
      return VSXTernaryLogic<127> (a, b, c);
    case 128:
      return VSXTernaryLogic<128> (a, b, c);
    case 129:
      return VSXTernaryLogic<129> (a, b, c);
    case 130:
      return VSXTernaryLogic<130> (a, b, c);
    case 131:
      return VSXTernaryLogic<131> (a, b, c);
    case 132:
      return VSXTernaryLogic<132> (a, b, c);
    case 133:
      return VSXTernaryLogic<133> (a, b, c);
    case 134:
      return VSXTernaryLogic<134> (a, b, c);
    case 135:
      return VSXTernaryLogic<135> (a, b, c);
    case 136:
      return VSXTernaryLogic<136> (a, b, c);
    case 137:
      return VSXTernaryLogic<137> (a, b, c);
    case 138:
      return VSXTernaryLogic<138> (a, b, c);
    case 139:
      return VSXTernaryLogic<139> (a, b, c);
    case 140:
      return VSXTernaryLogic<140> (a, b, c);
    case 141:
      return VSXTernaryLogic<141> (a, b, c);
    case 142:
      return VSXTernaryLogic<142> (a, b, c);
    case 143:
      return VSXTernaryLogic<143> (a, b, c);
    case 144:
      return VSXTernaryLogic<144> (a, b, c);
    case 145:
      return VSXTernaryLogic<145> (a, b, c);
    case 146:
      return VSXTernaryLogic<146> (a, b, c);
    case 147:
      return VSXTernaryLogic<147> (a, b, c);
    case 148:
      return VSXTernaryLogic<148> (a, b, c);
    case 149:
      return VSXTernaryLogic<149> (a, b, c);
    case 150:
      return VSXTernaryLogic<150> (a, b, c);
    case 151:
      return VSXTernaryLogic<151> (a, b, c);
    case 152:
      return VSXTernaryLogic<152> (a, b, c);
    case 153:
      return VSXTernaryLogic<153> (a, b, c);
    case 154:
      return VSXTernaryLogic<154> (a, b, c);
    case 155:
      return VSXTernaryLogic<155> (a, b, c);
    case 156:
      return VSXTernaryLogic<156> (a, b, c);
    case 157:
      return VSXTernaryLogic<157> (a, b, c);
    case 158:
      return VSXTernaryLogic<158> (a, b, c);
    case 159:
      return VSXTernaryLogic<159> (a, b, c);
    case 160:
      return VSXTernaryLogic<160> (a, b, c);
    case 161:
      return VSXTernaryLogic<161> (a, b, c);
    case 162:
      return VSXTernaryLogic<162> (a, b, c);
    case 163:
      return VSXTernaryLogic<163> (a, b, c);
    case 164:
      return VSXTernaryLogic<164> (a, b, c);
    case 165:
      return VSXTernaryLogic<165> (a, b, c);
    case 166:
      return VSXTernaryLogic<166> (a, b, c);
    case 167:
      return VSXTernaryLogic<167> (a, b, c);
    case 168:
      return VSXTernaryLogic<168> (a, b, c);
    case 169:
      return VSXTernaryLogic<169> (a, b, c);
    case 170:
      return VSXTernaryLogic<170> (a, b, c);
    case 171:
      return VSXTernaryLogic<171> (a, b, c);
    case 172:
      return VSXTernaryLogic<172> (a, b, c);
    case 173:
      return VSXTernaryLogic<173> (a, b, c);
    case 174:
      return VSXTernaryLogic<174> (a, b, c);
    case 175:
      return VSXTernaryLogic<175> (a, b, c);
    case 176:
      return VSXTernaryLogic<176> (a, b, c);
    case 177:
      return VSXTernaryLogic<177> (a, b, c);
    case 178:
      return VSXTernaryLogic<178> (a, b, c);
    case 179:
      return VSXTernaryLogic<179> (a, b, c);
    case 180:
      return VSXTernaryLogic<180> (a, b, c);
    case 181:
      return VSXTernaryLogic<181> (a, b, c);
    case 182:
      return VSXTernaryLogic<182> (a, b, c);
    case 183:
      return VSXTernaryLogic<183> (a, b, c);
    case 184:
      return VSXTernaryLogic<184> (a, b, c);
    case 185:
      return VSXTernaryLogic<185> (a, b, c);
    case 186:
      return VSXTernaryLogic<186> (a, b, c);
    case 187:
      return VSXTernaryLogic<187> (a, b, c);
    case 188:
      return VSXTernaryLogic<188> (a, b, c);
    case 189:
      return VSXTernaryLogic<189> (a, b, c);
    case 190:
      return VSXTernaryLogic<190> (a, b, c);
    case 191:
      return VSXTernaryLogic<191> (a, b, c);
    case 192:
      return VSXTernaryLogic<192> (a, b, c);
    case 193:
      return VSXTernaryLogic<193> (a, b, c);
    case 194:
      return VSXTernaryLogic<194> (a, b, c);
    case 195:
      return VSXTernaryLogic<195> (a, b, c);
    case 196:
      return VSXTernaryLogic<196> (a, b, c);
    case 197:
      return VSXTernaryLogic<197> (a, b, c);
    case 198:
      return VSXTernaryLogic<198> (a, b, c);
    case 199:
      return VSXTernaryLogic<199> (a, b, c);
    case 200:
      return VSXTernaryLogic<200> (a, b, c);
    case 201:
      return VSXTernaryLogic<201> (a, b, c);
    case 202:
      return VSXTernaryLogic<202> (a, b, c);
    case 203:
      return VSXTernaryLogic<203> (a, b, c);
    case 204:
      return VSXTernaryLogic<204> (a, b, c);
    case 205:
      return VSXTernaryLogic<205> (a, b, c);
    case 206:
      return VSXTernaryLogic<206> (a, b, c);
    case 207:
      return VSXTernaryLogic<207> (a, b, c);
    case 208:
      return VSXTernaryLogic<208> (a, b, c);
    case 209:
      return VSXTernaryLogic<209> (a, b, c);
    case 210:
      return VSXTernaryLogic<210> (a, b, c);
    case 211:
      return VSXTernaryLogic<211> (a, b, c);
    case 212:
      return VSXTernaryLogic<212> (a, b, c);
    case 213:
      return VSXTernaryLogic<213> (a, b, c);
    case 214:
      return VSXTernaryLogic<214> (a, b, c);
    case 215:
      return VSXTernaryLogic<215> (a, b, c);
    case 216:
      return VSXTernaryLogic<216> (a, b, c);
    case 217:
      return VSXTernaryLogic<217> (a, b, c);
    case 218:
      return VSXTernaryLogic<218> (a, b, c);
    case 219:
      return VSXTernaryLogic<219> (a, b, c);
    case 220:
      return VSXTernaryLogic<220> (a, b, c);
    case 221:
      return VSXTernaryLogic<221> (a, b, c);
    case 222:
      return VSXTernaryLogic<222> (a, b, c);
    case 223:
      return VSXTernaryLogic<223> (a, b, c);
    case 224:
      return VSXTernaryLogic<224> (a, b, c);
    case 225:
      return VSXTernaryLogic<225> (a, b, c);
    case 226:
      return VSXTernaryLogic<226> (a, b, c);
    case 227:
      return VSXTernaryLogic<227> (a, b, c);
    case 228:
      return VSXTernaryLogic<228> (a, b, c);
    case 229:
      return VSXTernaryLogic<229> (a, b, c);
    case 230:
      return VSXTernaryLogic<230> (a, b, c);
    case 231:
      return VSXTernaryLogic<231> (a, b, c);
    case 232:
      return VSXTernaryLogic<232> (a, b, c);
    case 233:
      return VSXTernaryLogic<233> (a, b, c);
    case 234:
      return VSXTernaryLogic<234> (a, b, c);
    case 235:
      return VSXTernaryLogic<235> (a, b, c);
    case 236:
      return VSXTernaryLogic<236> (a, b, c);
    case 237:
      return VSXTernaryLogic<237> (a, b, c);
    case 238:
      return VSXTernaryLogic<238> (a, b, c);
    case 239:
      return VSXTernaryLogic<239> (a, b, c);
    case 240:
      return VSXTernaryLogic<240> (a, b, c);
    case 241:
      return VSXTernaryLogic<241> (a, b, c);
    case 242:
      return VSXTernaryLogic<242> (a, b, c);
    case 243:
      return VSXTernaryLogic<243> (a, b, c);
    case 244:
      return VSXTernaryLogic<244> (a, b, c);
    case 245:
      return VSXTernaryLogic<245> (a, b, c);
    case 246:
      return VSXTernaryLogic<246> (a, b, c);
    case 247:
      return VSXTernaryLogic<247> (a, b, c);
    case 248:
      return VSXTernaryLogic<248> (a, b, c);
    case 249:
      return VSXTernaryLogic<249> (a, b, c);
    case 250:
      return VSXTernaryLogic<250> (a, b, c);
    case 251:
      return VSXTernaryLogic<251> (a, b, c);
    case 252:
      return VSXTernaryLogic<252> (a, b, c);
    case 253:
      return VSXTernaryLogic<253> (a, b, c);
    case 254:
      return VSXTernaryLogic<254> (a, b, c);
    case 255:
      return VSXTernaryLogic<255> (a, b, c);
    default:
      return a;
    }
}

int
main (int argc, char **argv)
{
  vector unsigned long long a = {0xD8, 0xDB};
  vector unsigned long long b = {0x6C, 0x6C};
  vector unsigned long long c = {0x56, 0x56};
  vector unsigned long long ternlog_result = VSXTernaryLogic (a, b, c, 0xB6);

  if (ternlog_result[0] != 0xffffffffffffff3dull
      || ternlog_result[1] != 0xffffffffffffff3eull)
    __builtin_abort ();

  return 0;
}
