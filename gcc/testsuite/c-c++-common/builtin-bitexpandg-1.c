/* Test __builtin_bitexpandg.  */
/* { dg-do run { target { c || c++11 } } } */
/* { dg-options "-O2" } */

#if !__has_builtin(__builtin_bitexpandg)
#error __builtin_bitexpandg unsupported
#endif

static_assert (__builtin_bitexpandg ((unsigned char) 0, (unsigned char) 0) == (unsigned char) 0, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0x123456789abcdefULL, (unsigned char) 0) == (unsigned char) 0, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0x123456789abcdefULL, (unsigned char) ~(unsigned char) 0) == (unsigned char) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0, (unsigned char) 0x123456789abcdefULL) == (unsigned char) 0, "");
static_assert (__builtin_bitexpandg ((unsigned char) ~(unsigned char) 0, (unsigned char) ~(unsigned char) 0) == (unsigned char) ~(unsigned char) 0, "");
static_assert (__builtin_bitexpandg ((unsigned char) ((((unsigned char) 1) << __builtin_popcountg ((unsigned char) 0x123456789abcdefULL)) - 1), (unsigned char) 0x123456789abcdefULL) == (unsigned char) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0xff, (unsigned char) 0xff) == 0xff, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0x0f, (unsigned char) 0xff) == 0x0f, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0x0f, (unsigned char) 0xf0) == 0xf0, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0xaaaaaaaaaaaaaaaaULL, (unsigned char) 0xccccccccccccccccULL) == (unsigned char) 0x8888888888888888ULL, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0x0f0f0f0f0f0f0f0fULL, (unsigned char) 0xaaaaaaaaaaaaaaaaULL) == (unsigned char) 0x00aa00aa00aa00aaULL, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0xd4, (unsigned char) 0xbf) == 0x94, "");
static_assert (__builtin_bitexpandg ((unsigned char) 0x66, (unsigned char) 0x1d) == 0x0c, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0, (unsigned short) 0) == (unsigned short) 0, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x123456789abcdefULL, (unsigned short) 0) == (unsigned short) 0, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x123456789abcdefULL, (unsigned short) ~(unsigned short) 0) == (unsigned short) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0, (unsigned short) 0x123456789abcdefULL) == (unsigned short) 0, "");
static_assert (__builtin_bitexpandg ((unsigned short) ~(unsigned short) 0, (unsigned short) ~(unsigned short) 0) == (unsigned short) ~(unsigned short) 0, "");
static_assert (__builtin_bitexpandg ((unsigned short) ((((unsigned short) 1) << __builtin_popcountg ((unsigned short) 0x123456789abcdefULL)) - 1), (unsigned short) 0x123456789abcdefULL) == (unsigned short) 0x123456789abcdefULL, "");
#if __CHAR_BIT__ * __SIZEOF_SHORT__ >= 16
static_assert (__builtin_bitexpandg ((unsigned short) 0x4646464646464646ULL, (unsigned short) 0x5555555555555555ULL) == (unsigned short) 0x1014101410141014ULL, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x1414141414141414ULL, (unsigned short) 0xaaaaaaaaaaaaaaaaULL) == (unsigned short) 0x0220022002200220ULL, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned short) 0xff, (unsigned short) 0xff) == 0xff, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x0f, (unsigned short) 0xff) == 0x0f, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x0f, (unsigned short) 0xf0) == 0xf0, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0xaaaaaaaaaaaaaaaaULL, (unsigned short) 0xccccccccccccccccULL) == (unsigned short) 0x8888888888888888ULL, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x0f0f0f0f0f0f0f0fULL, (unsigned short) 0xaaaaaaaaaaaaaaaaULL) == (unsigned short) 0x00aa00aa00aa00aaULL, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0xd4, (unsigned short) 0xbf) == 0x94, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0x66, (unsigned short) 0x1d) == 0x0c, "");
#if __CHAR_BIT__ * __SIZEOF_SHORT__ >= 16
static_assert (__builtin_bitexpandg ((unsigned short) 0xebd4U, (unsigned short) 0x93bfU) == (unsigned short) 0x1394, "");
static_assert (__builtin_bitexpandg ((unsigned short) 0xa166U, (unsigned short) 0x001dU) == (unsigned short) 0xc, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned int) 0, (unsigned int) 0) == (unsigned int) 0, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x123456789abcdefULL, (unsigned int) 0) == (unsigned int) 0, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x123456789abcdefULL, (unsigned int) ~(unsigned int) 0) == (unsigned int) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0, (unsigned int) 0x123456789abcdefULL) == (unsigned int) 0, "");
static_assert (__builtin_bitexpandg ((unsigned int) ~(unsigned int) 0, (unsigned int) ~(unsigned int) 0) == (unsigned int) ~(unsigned int) 0, "");
static_assert (__builtin_bitexpandg ((unsigned int) ((((unsigned int) 1) << __builtin_popcountg ((unsigned int) 0x123456789abcdefULL)) - 1), (unsigned int) 0x123456789abcdefULL) == (unsigned int) 0x123456789abcdefULL, "");
#if __CHAR_BIT__ * __SIZEOF_INT__ >= 16
static_assert (__builtin_bitexpandg ((unsigned int) 0x4646464646464646ULL, (unsigned int) 0x5555555555555555ULL) == (unsigned int) 0x1014101410141014ULL, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x1414141414141414ULL, (unsigned int) 0xaaaaaaaaaaaaaaaaULL) == (unsigned int) 0x0220022002200220ULL, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned int) 0xff, (unsigned int) 0xff) == 0xff, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x0f, (unsigned int) 0xff) == 0x0f, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x0f, (unsigned int) 0xf0) == 0xf0, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0xaaaaaaaaaaaaaaaaULL, (unsigned int) 0xccccccccccccccccULL) == (unsigned int) 0x8888888888888888ULL, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x0f0f0f0f0f0f0f0fULL, (unsigned int) 0xaaaaaaaaaaaaaaaaULL) == (unsigned int) 0x00aa00aa00aa00aaULL, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0xd4, (unsigned int) 0xbf) == 0x94, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x66, (unsigned int) 0x1d) == 0x0c, "");
#if __CHAR_BIT__ * __SIZEOF_INT__ >= 16
static_assert (__builtin_bitexpandg ((unsigned int) 0xebd4U, (unsigned int) 0x93bfU) == (unsigned int) 0x1394, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0xa166U, (unsigned int) 0x001dU) == (unsigned int) 0xc, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_INT__ >= 32
static_assert (__builtin_bitexpandg ((unsigned int) 0x9daeebd4UL, (unsigned int) 0xad8793bfUL) == (unsigned int) 0xa9851394UL, "");
static_assert (__builtin_bitexpandg ((unsigned int) 0x979ba166UL, (unsigned int) 0x54ca001dUL) == (unsigned int) 0x448000cUL, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned long) 0, (unsigned long) 0) == (unsigned long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x123456789abcdefULL, (unsigned long) 0) == (unsigned long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x123456789abcdefULL, (unsigned long) ~(unsigned long) 0) == (unsigned long) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0, (unsigned long) 0x123456789abcdefULL) == (unsigned long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long) ~(unsigned long) 0, (unsigned long) ~(unsigned long) 0) == (unsigned long) ~(unsigned long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long) ((((unsigned long) 1) << __builtin_popcountg ((unsigned long) 0x123456789abcdefULL)) - 1), (unsigned long) 0x123456789abcdefULL) == (unsigned long) 0x123456789abcdefULL, "");
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 16
static_assert (__builtin_bitexpandg ((unsigned long) 0x4646464646464646ULL, (unsigned long) 0x5555555555555555ULL) == (unsigned long) 0x1014101410141014ULL, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x1414141414141414ULL, (unsigned long) 0xaaaaaaaaaaaaaaaaULL) == (unsigned long) 0x0220022002200220ULL, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned long) 0xff, (unsigned long) 0xff) == 0xff, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x0f, (unsigned long) 0xff) == 0x0f, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x0f, (unsigned long) 0xf0) == 0xf0, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0xaaaaaaaaaaaaaaaaULL, (unsigned long) 0xccccccccccccccccULL) == (unsigned long) 0x8888888888888888ULL, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x0f0f0f0f0f0f0f0fULL, (unsigned long) 0xaaaaaaaaaaaaaaaaULL) == (unsigned long) 0x00aa00aa00aa00aaULL, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0xd4, (unsigned long) 0xbf) == 0x94, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x66, (unsigned long) 0x1d) == 0x0c, "");
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 16
static_assert (__builtin_bitexpandg ((unsigned long) 0xebd4U, (unsigned long) 0x93bfU) == (unsigned long) 0x1394, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0xa166U, (unsigned long) 0x001dU) == (unsigned long) 0xc, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 32
static_assert (__builtin_bitexpandg ((unsigned long) 0x9daeebd4UL, (unsigned long) 0xad8793bfUL) == (unsigned long) 0xa9851394UL, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0x979ba166UL, (unsigned long) 0x54ca001dUL) == (unsigned long) 0x448000cUL, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 64
static_assert (__builtin_bitexpandg ((unsigned long) 0x5bd30f069daeebd4ULL, (unsigned long) 0x64f9a89bad8793bfULL) == (unsigned long) 0x4a1a092a9851394ULL, "");
static_assert (__builtin_bitexpandg ((unsigned long) 0xb5672499979ba166ULL, (unsigned long) 0xd315782354ca001dULL) == (unsigned long) 0xd01470200448000cULL, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned long long) 0, (unsigned long long) 0) == (unsigned long long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x123456789abcdefULL, (unsigned long long) 0) == (unsigned long long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x123456789abcdefULL, (unsigned long long) ~(unsigned long long) 0) == (unsigned long long) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0, (unsigned long long) 0x123456789abcdefULL) == (unsigned long long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long long) ~(unsigned long long) 0, (unsigned long long) ~(unsigned long long) 0) == (unsigned long long) ~(unsigned long long) 0, "");
static_assert (__builtin_bitexpandg ((unsigned long long) ((((unsigned long long) 1) << __builtin_popcountg ((unsigned long long) 0x123456789abcdefULL)) - 1), (unsigned long long) 0x123456789abcdefULL) == (unsigned long long) 0x123456789abcdefULL, "");
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 16
static_assert (__builtin_bitexpandg ((unsigned long long) 0x4646464646464646ULL, (unsigned long long) 0x5555555555555555ULL) == (unsigned long long) 0x1014101410141014ULL, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x1414141414141414ULL, (unsigned long long) 0xaaaaaaaaaaaaaaaaULL) == (unsigned long long) 0x0220022002200220ULL, "");
#endif
static_assert (__builtin_bitexpandg ((unsigned long long) 0xff, (unsigned long long) 0xff) == 0xff, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x0f, (unsigned long long) 0xff) == 0x0f, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x0f, (unsigned long long) 0xf0) == 0xf0, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0xaaaaaaaaaaaaaaaaULL, (unsigned long long) 0xccccccccccccccccULL) == (unsigned long long) 0x8888888888888888ULL, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x0f0f0f0f0f0f0f0fULL, (unsigned long long) 0xaaaaaaaaaaaaaaaaULL) == (unsigned long long) 0x00aa00aa00aa00aaULL, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0xd4, (unsigned long long) 0xbf) == 0x94, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x66, (unsigned long long) 0x1d) == 0x0c, "");
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 16
static_assert (__builtin_bitexpandg ((unsigned long long) 0xebd4U, (unsigned long long) 0x93bfU) == (unsigned long long) 0x1394, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0xa166U, (unsigned long long) 0x001dU) == (unsigned long long) 0xc, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 32
static_assert (__builtin_bitexpandg ((unsigned long long) 0x9daeebd4UL, (unsigned long long) 0xad8793bfUL) == (unsigned long long) 0xa9851394UL, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0x979ba166UL, (unsigned long long) 0x54ca001dUL) == (unsigned long long) 0x448000cUL, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 64
static_assert (__builtin_bitexpandg ((unsigned long long) 0x5bd30f069daeebd4ULL, (unsigned long long) 0x64f9a89bad8793bfULL) == (unsigned long long) 0x4a1a092a9851394ULL, "");
static_assert (__builtin_bitexpandg ((unsigned long long) 0xb5672499979ba166ULL, (unsigned long long) 0xd315782354ca001dULL) == (unsigned long long) 0xd01470200448000cULL, "");
#endif
#ifdef __SIZEOF_INT128__
static_assert (__builtin_bitexpandg ((unsigned __int128) 0, (unsigned __int128) 0) == (unsigned __int128) 0, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x123456789abcdefULL, (unsigned __int128) 0) == (unsigned __int128) 0, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x123456789abcdefULL, (unsigned __int128) ~(unsigned __int128) 0) == (unsigned __int128) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0, (unsigned __int128) 0x123456789abcdefULL) == (unsigned __int128) 0, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) ~(unsigned __int128) 0, (unsigned __int128) ~(unsigned __int128) 0) == (unsigned __int128) ~(unsigned __int128) 0, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) ((((unsigned __int128) 1) << __builtin_popcountg ((unsigned __int128) 0x123456789abcdefULL)) - 1), (unsigned __int128) 0x123456789abcdefULL) == (unsigned __int128) 0x123456789abcdefULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x4646464646464646ULL, (unsigned __int128) 0x5555555555555555ULL) == (unsigned __int128) 0x1014101410141014ULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x1414141414141414ULL, (unsigned __int128) 0xaaaaaaaaaaaaaaaaULL) == (unsigned __int128) 0x0220022002200220ULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0xff, (unsigned __int128) 0xff) == 0xff, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x0f, (unsigned __int128) 0xff) == 0x0f, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x0f, (unsigned __int128) 0xf0) == 0xf0, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0xaaaaaaaaaaaaaaaaULL, (unsigned __int128) 0xccccccccccccccccULL) == (unsigned __int128) 0x8888888888888888ULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x0f0f0f0f0f0f0f0fULL, (unsigned __int128) 0xaaaaaaaaaaaaaaaaULL) == (unsigned __int128) 0x00aa00aa00aa00aaULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0xd4, (unsigned __int128) 0xbf) == 0x94, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x66, (unsigned __int128) 0x1d) == 0x0c, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0xebd4U, (unsigned __int128) 0x93bfU) == (unsigned __int128) 0x1394, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0xa166U, (unsigned __int128) 0x001dU) == (unsigned __int128) 0xc, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x9daeebd4UL, (unsigned __int128) 0xad8793bfUL) == (unsigned __int128) 0xa9851394UL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x979ba166UL, (unsigned __int128) 0x54ca001dUL) == (unsigned __int128) 0x448000cUL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0x5bd30f069daeebd4ULL, (unsigned __int128) 0x64f9a89bad8793bfULL) == (unsigned __int128) 0x4a1a092a9851394ULL, "");
static_assert (__builtin_bitexpandg ((unsigned __int128) 0xb5672499979ba166ULL, (unsigned __int128) 0xd315782354ca001dULL) == (unsigned __int128) 0xd01470200448000cULL, "");
#endif
#if __BITINT_MAXWIDTH__ >= 256
static_assert (__builtin_bitexpandg ((unsigned _BitInt(256)) 2977712650221868789329368963712957747535884589227943441826207435705422199671uwb,
				     (unsigned _BitInt(256)) 71392704391611725846964173788815479408492634097466601526619781877369006838376uwb)
	       == 6106334322603120698343087248751001114229967022894482914225634220926378496104uwb);
static_assert (__builtin_bitexpandg ((unsigned _BitInt(256)) 71392704391611725846964173788815479408492634097466601526619781877369006838376uwb,
				     (unsigned _BitInt(256)) 2977712650221868789329368963712957747535884589227943441826207435705422199671uwb)
	       == 2715653213312698747932362475061475316365454769578495772994929980809745011024uwb);
#endif
#if __BITINT_MAXWIDTH__ >= 575
static_assert (__builtin_bitexpandg ((unsigned _BitInt(575)) 118131541925402402155086157499115656285751034069848007471551618267237710886541403235442402972757841211601305512508765941610080588540585990221962870251502405139968332129617187uwb,
				     (unsigned _BitInt(575)) 23115097167410578660280364356726488934202319035287675841474505237120386080336714513506555994051218240174733511200356850940493563860321836251689717522161899716121398557567847uwb)
	       == 7623895243663960691569173075814666314012124130457458291983897390462352361781036204975171902420126018071090572104100014023386872050266171372969434719876916563037514178240771uwb);
static_assert (__builtin_bitexpandg ((unsigned _BitInt(575)) 23115097167410578660280364356726488934202319035287675841474505237120386080336714513506555994051218240174733511200356850940493563860321836251689717522161899716121398557567847uwb,
				     (unsigned _BitInt(575)) 118131541925402402155086157499115656285751034069848007471551618267237710886541403235442402972757841211601305512508765941610080588540585990221962870251502405139968332129617187uwb)
	       == 56294931635403572486906377531414566093300447783103849622139379559580554463924172104226430624225347319693207529072644684462926168000686187753501958709265974532503455809323043uwb);
#endif
#if __BITINT_MAXWIDTH__ >= 1027
static_assert (__builtin_bitexpandg ((unsigned _BitInt(1027)) 294473855873745241271551195901386950656729759273365562206638967800992654698032636902920911003782368139677793592762040049443703439321090202240002644548914408256861393744782128292349394935235968569736234857665376806506152875996067378466482077335451170878303723908011096841794981692284316910893612604487249472311uwb,
				     (unsigned _BitInt(1027)) 540548418560750924575664703872106986534265345396756980894364393357877823859721129540519729973560861792682263121263569358086814437568179760344168293628388053637218170474254474264275367843920704704472260655775742000715646811343262012513538736719387133499156091882497205971330308173485560590613388499248986333824uwb)
	       == 1053432602631601823738458435352232608180994474339675507824798318815508385108517845015392672754706461608810238033305170246166427250962973734897418531868110631156100837780702613467177557639965349351804007196695316749042516880235247409566358718996737210967041320563094935743638949079055891438475704252653642368uwb);
static_assert (__builtin_bitexpandg ((unsigned _BitInt(1027)) 540548418560750924575664703872106986534265345396756980894364393357877823859721129540519729973560861792682263121263569358086814437568179760344168293628388053637218170474254474264275367843920704704472260655775742000715646811343262012513538736719387133499156091882497205971330308173485560590613388499248986333824uwb,
				     (unsigned _BitInt(1027)) 294473855873745241271551195901386950656729759273365562206638967800992654698032636902920911003782368139677793592762040049443703439321090202240002644548914408256861393744782128292349394935235968569736234857665376806506152875996067378466482077335451170878303723908011096841794981692284316910893612604487249472311uwb)
	       == 91355371957256612074255238122359885881282861027030297292884885291983886368774408697841433225727842530160483371399389894560044416587561512148347308387668958225153909170929379941956797733013538101336868524683698284851251557537245561688655263088925983144365058547397146385467295975230358276694698660737092454400uwb);
#endif

[[gnu::noipa]] unsigned char
bitexpand8 (unsigned char x, unsigned char m)
{
  return __builtin_bitexpandg (x, m);
}

[[gnu::noipa]] unsigned short
bitexpand16 (unsigned short x, unsigned short m)
{
  return __builtin_bitexpandg (x, m);
}

[[gnu::noipa]] unsigned int
bitexpand32 (unsigned int x, unsigned int m)
{
  return __builtin_bitexpandg (x, m);
}

[[gnu::noipa]] unsigned long
bitexpandl (unsigned long x, unsigned long m)
{
  return __builtin_bitexpandg (x, m);
}

[[gnu::noipa]] unsigned long long
bitexpand64 (unsigned long long x, unsigned long long m)
{
  return __builtin_bitexpandg (x, m);
}

#ifdef __SIZEOF_INT128__
[[gnu::noipa]] unsigned __int128
bitexpand128 (unsigned __int128 x, unsigned __int128 m)
{
  return __builtin_bitexpandg (x, m);
}
#endif

#if __BITINT_MAXWIDTH__ >= 256
[[gnu::noipa]] unsigned _BitInt(256)
bitexpand256 (unsigned _BitInt(256) x, unsigned _BitInt(256) y)
{
  return __builtin_bitexpandg (x, y);
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
[[gnu::noipa]] unsigned _BitInt(575)
bitexpand575 (unsigned _BitInt(575) x, unsigned _BitInt(575) y)
{
  return __builtin_bitexpandg (x, y);
}
#endif

#if __BITINT_MAXWIDTH__ >= 1027
[[gnu::noipa]] unsigned _BitInt(1027)
bitexpand1027 (unsigned _BitInt(1027) x, unsigned _BitInt(1027) y)
{
  return __builtin_bitexpandg (x, y);
}
#endif

#ifdef __SIZEOF_INT128__
#define T unsigned __int128
#else
#define T unsigned long long
#endif

[[gnu::noipa]] T
bitexpand8w (T x, T m)
{
  return bitexpand8 (x, m);
}

[[gnu::noipa]] T
bitexpand16w (T x, T m)
{
  return bitexpand16 (x, m);
}

[[gnu::noipa]] T
bitexpand32w (T x, T m)
{
  return bitexpand32 (x, m);
}

[[gnu::noipa]] T
bitexpandlw (T x, T m)
{
  return bitexpandl (x, m);
}

[[gnu::noipa]] T
bitexpand64w (T x, T m)
{
  return bitexpand64 (x, m);
}

int
main ()
{
  struct { T (*fn) (T, T); unsigned int digits; } tests[] = {
    { bitexpand8w, __CHAR_BIT__ },
    { bitexpand16w, __CHAR_BIT__ * __SIZEOF_SHORT__ },
    { bitexpand32w, __CHAR_BIT__ * __SIZEOF_INT__ },
    { bitexpandlw, __CHAR_BIT__ * __SIZEOF_LONG__ },
    { bitexpand64w, __CHAR_BIT__ * __SIZEOF_LONG_LONG__ },
#ifdef __SIZEOF_INT128__
    { bitexpand128, __CHAR_BIT__ * __SIZEOF_INT128__ },
#endif
  };
  for (int i = 0; i < sizeof (tests) / sizeof (tests[0]); ++i)
    {
      T mask = ~(T) 0;
      if (tests[i].digits < sizeof (mask) * __CHAR_BIT__)
	mask >>= sizeof (mask) * __CHAR_BIT__ - tests[i].digits;
      if (tests[i].fn (0, 0) != 0)
	__builtin_abort ();
      if (tests[i].fn (0x123456789abcdefULL, 0) != 0)
	__builtin_abort ();
      if (tests[i].fn (0x123456789abcdefULL, ~0ULL) != (0x123456789abcdefULL & mask))
	__builtin_abort ();
      if (tests[i].fn (0, 0x123456789abcdefULL) != 0)
	__builtin_abort ();
      if (tests[i].fn (~0ULL, ~0ULL) != (~0ULL & mask))
	__builtin_abort ();
      if (tests[i].fn ((1ULL << __builtin_popcountg (0x123456789abcdefULL & mask)) - 1,
		       0x123456789abcdefULL) != (0x123456789abcdefULL & mask))
	__builtin_abort ();
      if (tests[i].digits >= 16)
	{
	  if (tests[i].fn (0x4646464646464646ULL, 0x5555555555555555ULL)
	      != (0x1014101410141014ULL & mask))
	    __builtin_abort ();
	  if (tests[i].fn (0x1414141414141414ULL, 0xaaaaaaaaaaaaaaaaULL)
	      != (0x0220022002200220ULL & mask))
	    __builtin_abort ();
	}
      if (tests[i].fn (0xff, 0xff) != 0xff)
	__builtin_abort ();
      if (tests[i].fn (0x0f, 0xff) != 0x0f)
	__builtin_abort ();
      if (tests[i].fn (0x0f, 0xf0) != 0xf0)
	__builtin_abort ();
      if (tests[i].fn (0xaaaaaaaaaaaaaaaaULL, 0xccccccccccccccccULL)
	  != (0x8888888888888888ULL & mask))
	__builtin_abort ();
      if (tests[i].fn (0x0f0f0f0f0f0f0f0fULL, 0xaaaaaaaaaaaaaaaaULL)
	  != (0x00aa00aa00aa00aaULL & mask))
	__builtin_abort ();
      if (tests[i].fn (0xd4, 0xbf) != 0x94)
	__builtin_abort ();
      if (tests[i].fn (0x66, 0x1d) != 0x0c)
	__builtin_abort ();
      if (tests[i].digits < 16)
	continue;
      if (tests[i].fn (0xebd4U, 0x93bfU) != 0x1394)
	__builtin_abort ();
      if (tests[i].fn (0xa166U, 0x001dU) != 0xc)
	__builtin_abort ();
      if (tests[i].digits < 32)
	continue;
      if (tests[i].fn (0x9daeebd4UL, 0xad8793bfUL) != 0xa9851394UL)
	__builtin_abort ();
      if (tests[i].fn (0x979ba166UL, 0x54ca001dUL) != 0x448000cUL)
	__builtin_abort ();
      if (tests[i].digits < 64)
	continue;
      if (tests[i].fn (0x5bd30f069daeebd4ULL, 0x64f9a89bad8793bfULL) != 0x4a1a092a9851394ULL)
	__builtin_abort ();
      if (tests[i].fn (0xb5672499979ba166ULL, 0xd315782354ca001dULL) != 0xd01470200448000cULL)
	__builtin_abort ();
    }
#if __BITINT_MAXWIDTH__ >= 256
  {
    const unsigned _BitInt(256) a = 2977712650221868789329368963712957747535884589227943441826207435705422199671uwb;
    const unsigned _BitInt(256) b = 71392704391611725846964173788815479408492634097466601526619781877369006838376uwb;
    if (bitexpand256 (a, b) != __builtin_bitexpandg (a, b)
	|| bitexpand256 (-1, -1) != -1
	|| bitexpand256 (0, 0) != 0
	|| bitexpand256 (b, a) != __builtin_bitexpandg (b, a))
      __builtin_abort ();
  }
#endif
#if __BITINT_MAXWIDTH__ >= 575
  {
    const unsigned _BitInt(575) a = 118131541925402402155086157499115656285751034069848007471551618267237710886541403235442402972757841211601305512508765941610080588540585990221962870251502405139968332129617187uwb;
    const unsigned _BitInt(575) b = 23115097167410578660280364356726488934202319035287675841474505237120386080336714513506555994051218240174733511200356850940493563860321836251689717522161899716121398557567847uwb;
    if (bitexpand575 (a, b) != __builtin_bitexpandg (a, b)
	|| bitexpand575 (-1, -1) != -1
	|| bitexpand575 (0, 0) != 0
	|| bitexpand575 (b, a) != __builtin_bitexpandg (b, a))
      __builtin_abort ();
  }
#endif
#if __BITINT_MAXWIDTH__ >= 1027
  {
    const unsigned _BitInt(1027) a = 294473855873745241271551195901386950656729759273365562206638967800992654698032636902920911003782368139677793592762040049443703439321090202240002644548914408256861393744782128292349394935235968569736234857665376806506152875996067378466482077335451170878303723908011096841794981692284316910893612604487249472311uwb;
    const unsigned _BitInt(1027) b = 540548418560750924575664703872106986534265345396756980894364393357877823859721129540519729973560861792682263121263569358086814437568179760344168293628388053637218170474254474264275367843920704704472260655775742000715646811343262012513538736719387133499156091882497205971330308173485560590613388499248986333824uwb;
    if (bitexpand1027 (a, b) != __builtin_bitexpandg (a, b)
	|| bitexpand1027 (-1, -1) != -1
	|| bitexpand1027 (0, 0) != 0
	|| bitexpand1027 (b, a) != __builtin_bitexpandg (b, a))
      __builtin_abort ();
  }
#endif
}
