/* Test __builtin_bitcompressg.  */
/* { dg-do run { target { c || c++11 } } } */
/* { dg-options "-O2" } */

#if !__has_builtin(__builtin_bitcompressg)
#error __builtin_bitcompressg unsupported
#endif

static_assert (__builtin_bitcompressg ((unsigned char) 0, (unsigned char) 0) == (unsigned char) 0, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0x123456789abcdefULL, (unsigned char) 0) == (unsigned char) 0, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0x123456789abcdefULL, (unsigned char) ~(unsigned char) 0) == (unsigned char) 0x123456789abcdefULL, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0, (unsigned char) 0x123456789abcdefULL) == (unsigned char) 0, "");
static_assert (__builtin_bitcompressg ((unsigned char) ~(unsigned char) 0, (unsigned char) ~(unsigned char) 0) == (unsigned char) ~(unsigned char) 0, "");
static_assert (__builtin_bitcompressg ((unsigned char) ~(unsigned char) 0, (unsigned char) 0x123456789abcdefULL) == (((unsigned char) 1) << __builtin_popcountg ((unsigned char) 0x123456789abcdefULL)) - 1, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0xff, (unsigned char) 0xff) == (unsigned char) 0xff, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0xff, (unsigned char) 0x0f) == (unsigned char) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0xff, (unsigned char) 0xf0) == (unsigned char) 0x0f, "");
#if (__CHAR_BIT__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned char) 0xaaaaaaaaaaaaaaaaULL, (unsigned char) 0xccccccccccccccccULL) == ((unsigned char) 0xaaaaaaaaaaaaaaaaULL) >> (__CHAR_BIT__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned char) ~(unsigned char) 0, (unsigned char) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned char) ~(unsigned char) 0) >> (__CHAR_BIT__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned char) 0xd4, (unsigned char) 0xbf) == (unsigned char) 0x54, "");
static_assert (__builtin_bitcompressg ((unsigned char) 0x66, (unsigned char) 0x1d) == (unsigned char) 0x02, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0, (unsigned short) 0) == (unsigned short) 0, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0x123456789abcdefULL, (unsigned short) 0) == (unsigned short) 0, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0x123456789abcdefULL, (unsigned short) ~(unsigned short) 0) == (unsigned short) 0x123456789abcdefULL, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0, (unsigned short) 0x123456789abcdefULL) == (unsigned short) 0, "");
static_assert (__builtin_bitcompressg ((unsigned short) ~(unsigned short) 0, (unsigned short) ~(unsigned short) 0) == (unsigned short) ~(unsigned short) 0, "");
static_assert (__builtin_bitcompressg ((unsigned short) ~(unsigned short) 0, (unsigned short) 0x123456789abcdefULL) == (((unsigned short) 1) << __builtin_popcountg ((unsigned short) 0x123456789abcdefULL)) - 1, "");
#if __CHAR_BIT__ * __SIZEOF_SHORT__ >= 16 && (__CHAR_BIT__ * __SIZEOF_SHORT__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned short) 0x1234123412341234ULL, (unsigned short) 0x5555555555555555ULL) == ((unsigned short) 0x4646464646464646ULL) >> (__CHAR_BIT__ * __SIZEOF_SHORT__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned short) 0x1234123412341234ULL, (unsigned short) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned short) 0x1414141414141414ULL) >> (__CHAR_BIT__ * __SIZEOF_SHORT__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned short) 0xff, (unsigned short) 0xff) == (unsigned short) 0xff, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0xff, (unsigned short) 0x0f) == (unsigned short) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0xff, (unsigned short) 0xf0) == (unsigned short) 0x0f, "");
#if (__CHAR_BIT__ * __SIZEOF_SHORT__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned short) 0xaaaaaaaaaaaaaaaaULL, (unsigned short) 0xccccccccccccccccULL) == ((unsigned short) 0xaaaaaaaaaaaaaaaaULL) >> (__CHAR_BIT__ * __SIZEOF_SHORT__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned short) ~(unsigned short) 0, (unsigned short) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned short) ~(unsigned short) 0) >> (__CHAR_BIT__ * __SIZEOF_SHORT__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned short) 0xd4, (unsigned short) 0xbf) == (unsigned short) 0x54, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0x66, (unsigned short) 0x1d) == (unsigned short) 0x02, "");
#if __CHAR_BIT__ * __SIZEOF_SHORT__ >= 16
static_assert (__builtin_bitcompressg ((unsigned short) 0xebd4U, (unsigned short) 0x93bfU) == (unsigned short) 0x05d4, "");
static_assert (__builtin_bitcompressg ((unsigned short) 0xa166U, (unsigned short) 0x001dU) == (unsigned short) 2, "");
#endif
static_assert (__builtin_bitcompressg ((unsigned int) 0, (unsigned int) 0) == (unsigned int) 0, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0x123456789abcdefULL, (unsigned int) 0) == (unsigned int) 0, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0x123456789abcdefULL, (unsigned int) ~(unsigned int) 0) == (unsigned int) 0x123456789abcdefULL, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0, (unsigned int) 0x123456789abcdefULL) == (unsigned int) 0, "");
static_assert (__builtin_bitcompressg ((unsigned int) ~(unsigned int) 0, (unsigned int) ~(unsigned int) 0) == (unsigned int) ~(unsigned int) 0, "");
static_assert (__builtin_bitcompressg ((unsigned int) ~(unsigned int) 0, (unsigned int) 0x123456789abcdefULL) == (((unsigned int) 1) << __builtin_popcountg ((unsigned int) 0x123456789abcdefULL)) - 1, "");
#if __CHAR_BIT__ * __SIZEOF_INT__ >= 16 && (__CHAR_BIT__ * __SIZEOF_INT__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned int) 0x1234123412341234ULL, (unsigned int) 0x5555555555555555ULL) == ((unsigned int) 0x4646464646464646ULL) >> (__CHAR_BIT__ * __SIZEOF_INT__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned int) 0x1234123412341234ULL, (unsigned int) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned int) 0x1414141414141414ULL) >> (__CHAR_BIT__ * __SIZEOF_INT__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned int) 0xff, (unsigned int) 0xff) == (unsigned int) 0xff, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0xff, (unsigned int) 0x0f) == (unsigned int) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0xff, (unsigned int) 0xf0) == (unsigned int) 0x0f, "");
#if (__CHAR_BIT__ * __SIZEOF_INT__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned int) 0xaaaaaaaaaaaaaaaaULL, (unsigned int) 0xccccccccccccccccULL) == ((unsigned int) 0xaaaaaaaaaaaaaaaaULL) >> (__CHAR_BIT__ * __SIZEOF_INT__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned int) ~(unsigned int) 0, (unsigned int) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned int) ~(unsigned int) 0) >> (__CHAR_BIT__ * __SIZEOF_INT__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned int) 0xd4, (unsigned int) 0xbf) == (unsigned int) 0x54, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0x66, (unsigned int) 0x1d) == (unsigned int) 0x02, "");
#if __CHAR_BIT__ * __SIZEOF_INT__ >= 16
static_assert (__builtin_bitcompressg ((unsigned int) 0xebd4U, (unsigned int) 0x93bfU) == (unsigned int) 0x05d4, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0xa166U, (unsigned int) 0x001dU) == (unsigned int) 2, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_INT__ >= 32
static_assert (__builtin_bitcompressg ((unsigned int) 0x9daeebd4U, (unsigned int) 0xad8793bfU) == (unsigned int) 0xbf5d4U, "");
static_assert (__builtin_bitcompressg ((unsigned int) 0x979ba166U, (unsigned int) 0x54ca001dU) == (unsigned int) 0x3b2U, "");
#endif
static_assert (__builtin_bitcompressg ((unsigned long) 0, (unsigned long) 0) == (unsigned long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0x123456789abcdefULL, (unsigned long) 0) == (unsigned long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0x123456789abcdefULL, (unsigned long) ~(unsigned long) 0) == (unsigned long) 0x123456789abcdefULL, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0, (unsigned long) 0x123456789abcdefULL) == (unsigned long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long) ~(unsigned long) 0, (unsigned long) ~(unsigned long) 0) == (unsigned long) ~(unsigned long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long) ~(unsigned long) 0, (unsigned long) 0x123456789abcdefULL) == (((unsigned long) 1) << __builtin_popcountg ((unsigned long) 0x123456789abcdefULL)) - 1, "");
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 16 && (__CHAR_BIT__ * __SIZEOF_LONG__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned long) 0x1234123412341234ULL, (unsigned long) 0x5555555555555555ULL) == ((unsigned long) 0x4646464646464646ULL) >> (__CHAR_BIT__ * __SIZEOF_LONG__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned long) 0x1234123412341234ULL, (unsigned long) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned long) 0x1414141414141414ULL) >> (__CHAR_BIT__ * __SIZEOF_LONG__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned long) 0xff, (unsigned long) 0xff) == (unsigned long) 0xff, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0xff, (unsigned long) 0x0f) == (unsigned long) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0xff, (unsigned long) 0xf0) == (unsigned long) 0x0f, "");
#if (__CHAR_BIT__ * __SIZEOF_LONG__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned long) 0xaaaaaaaaaaaaaaaaULL, (unsigned long) 0xccccccccccccccccULL) == ((unsigned long) 0xaaaaaaaaaaaaaaaaULL) >> (__CHAR_BIT__ * __SIZEOF_LONG__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned long) ~(unsigned long) 0, (unsigned long) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned long) ~(unsigned long) 0) >> (__CHAR_BIT__ * __SIZEOF_LONG__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned long) 0xd4, (unsigned long) 0xbf) == (unsigned long) 0x54, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0x66, (unsigned long) 0x1d) == (unsigned long) 0x02, "");
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 16
static_assert (__builtin_bitcompressg ((unsigned long) 0xebd4U, (unsigned long) 0x93bfU) == (unsigned long) 0x05d4, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0xa166U, (unsigned long) 0x001dU) == (unsigned long) 2, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 32
static_assert (__builtin_bitcompressg ((unsigned long) 0x9daeebd4U, (unsigned long) 0xad8793bfU) == (unsigned long) 0xbf5d4U, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0x979ba166U, (unsigned long) 0x54ca001dU) == (unsigned long) 0x3b2U, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG__ >= 64
static_assert (__builtin_bitcompressg ((unsigned long) 0x5bd30f069daeebd4ULL, (unsigned long) 0x64f9a89bad8793bfULL) == (unsigned long) 0x13522bf5d4ULL, "");
static_assert (__builtin_bitcompressg ((unsigned long) 0xb5672499979ba166ULL, (unsigned long) 0xd315782354ca001dULL) == (unsigned long) 0x2ad0bb2ULL, "");
#endif
static_assert (__builtin_bitcompressg ((unsigned long long) 0, (unsigned long long) 0) == (unsigned long long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0x123456789abcdefULL, (unsigned long long) 0) == (unsigned long long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0x123456789abcdefULL, (unsigned long long) ~(unsigned long long) 0) == (unsigned long long) 0x123456789abcdefULL, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0, (unsigned long long) 0x123456789abcdefULL) == (unsigned long long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long long) ~(unsigned long long) 0, (unsigned long long) ~(unsigned long long) 0) == (unsigned long long) ~(unsigned long long) 0, "");
static_assert (__builtin_bitcompressg ((unsigned long long) ~(unsigned long long) 0, (unsigned long long) 0x123456789abcdefULL) == (((unsigned long long) 1) << __builtin_popcountg ((unsigned long long) 0x123456789abcdefULL)) - 1, "");
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 16 && (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned long long) 0x1234123412341234ULL, (unsigned long long) 0x5555555555555555ULL) == ((unsigned long long) 0x4646464646464646ULL) >> (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0x1234123412341234ULL, (unsigned long long) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned long long) 0x1414141414141414ULL) >> (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned long long) 0xff, (unsigned long long) 0xff) == (unsigned long long) 0xff, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0xff, (unsigned long long) 0x0f) == (unsigned long long) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0xff, (unsigned long long) 0xf0) == (unsigned long long) 0x0f, "");
#if (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ % 2) == 0
static_assert (__builtin_bitcompressg ((unsigned long long) 0xaaaaaaaaaaaaaaaaULL, (unsigned long long) 0xccccccccccccccccULL) == ((unsigned long long) 0xaaaaaaaaaaaaaaaaULL) >> (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ / 2), "");
static_assert (__builtin_bitcompressg ((unsigned long long) ~(unsigned long long) 0, (unsigned long long) 0xaaaaaaaaaaaaaaaaULL) == ((unsigned long long) ~(unsigned long long) 0) >> (__CHAR_BIT__ * __SIZEOF_LONG_LONG__ / 2), "");
#endif
static_assert (__builtin_bitcompressg ((unsigned long long) 0xd4, (unsigned long long) 0xbf) == (unsigned long long) 0x54, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0x66, (unsigned long long) 0x1d) == (unsigned long long) 0x02, "");
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 16
static_assert (__builtin_bitcompressg ((unsigned long long) 0xebd4U, (unsigned long long) 0x93bfU) == (unsigned long long) 0x05d4, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0xa166U, (unsigned long long) 0x001dU) == (unsigned long long) 2, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 32
static_assert (__builtin_bitcompressg ((unsigned long long) 0x9daeebd4U, (unsigned long long) 0xad8793bfU) == (unsigned long long) 0xbf5d4U, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0x979ba166U, (unsigned long long) 0x54ca001dU) == (unsigned long long) 0x3b2U, "");
#endif
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ >= 64
static_assert (__builtin_bitcompressg ((unsigned long long) 0x5bd30f069daeebd4ULL, (unsigned long long) 0x64f9a89bad8793bfULL) == (unsigned long long) 0x13522bf5d4ULL, "");
static_assert (__builtin_bitcompressg ((unsigned long long) 0xb5672499979ba166ULL, (unsigned long long) 0xd315782354ca001dULL) == (unsigned long long) 0x2ad0bb2ULL, "");
#endif
#ifdef __SIZEOF_INT128__
static_assert (__builtin_bitcompressg ((unsigned __int128) 0, (unsigned __int128) 0) == (unsigned __int128) 0, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0x123456789abcdefULL, (unsigned __int128) 0) == (unsigned __int128) 0, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0x123456789abcdefULL, (unsigned __int128) ~(unsigned __int128) 0) == (unsigned __int128) 0x123456789abcdefULL, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0, (unsigned __int128) 0x123456789abcdefULL) == (unsigned __int128) 0, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) ~(unsigned __int128) 0, (unsigned __int128) ~(unsigned __int128) 0) == (unsigned __int128) ~(unsigned __int128) 0, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) ~(unsigned __int128) 0, (unsigned __int128) 0x123456789abcdefULL) == (((unsigned __int128) 1) << __builtin_popcountg ((unsigned __int128) 0x123456789abcdefULL)) - 1, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xff, (unsigned __int128) 0xff) == (unsigned __int128) 0xff, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xff, (unsigned __int128) 0x0f) == (unsigned __int128) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xff, (unsigned __int128) 0xf0) == (unsigned __int128) 0x0f, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xd4, (unsigned __int128) 0xbf) == (unsigned __int128) 0x54, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0x66, (unsigned __int128) 0x1d) == (unsigned __int128) 0x02, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xebd4U, (unsigned __int128) 0x93bfU) == (unsigned __int128) 0x05d4, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xa166U, (unsigned __int128) 0x001dU) == (unsigned __int128) 2, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0x9daeebd4U, (unsigned __int128) 0xad8793bfU) == (unsigned __int128) 0xbf5d4U, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0x979ba166U, (unsigned __int128) 0x54ca001dU) == (unsigned __int128) 0x3b2U, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0x5bd30f069daeebd4ULL, (unsigned __int128) 0x64f9a89bad8793bfULL) == (unsigned __int128) 0x13522bf5d4ULL, "");
static_assert (__builtin_bitcompressg ((unsigned __int128) 0xb5672499979ba166ULL, (unsigned __int128) 0xd315782354ca001dULL) == (unsigned __int128) 0x2ad0bb2ULL, "");
#endif
#if __BITINT_MAXWIDTH__ >= 256
static_assert (__builtin_bitcompressg ((unsigned _BitInt(256)) 2977712650221868789329368963712957747535884589227943441826207435705422199671uwb,
				       (unsigned _BitInt(256)) 71392704391611725846964173788815479408492634097466601526619781877369006838376uwb)
	       == 7351178182929446285521030097634422236446uwb);
static_assert (__builtin_bitcompressg ((unsigned _BitInt(256)) 71392704391611725846964173788815479408492634097466601526619781877369006838376uwb,
				       (unsigned _BitInt(256)) 2977712650221868789329368963712957747535884589227943441826207435705422199671uwb)
	       == 7792001651588212291781843281756166832uwb);
#endif
#if __BITINT_MAXWIDTH__ >= 575
static_assert (__builtin_bitcompressg ((unsigned _BitInt(575)) 118131541925402402155086157499115656285751034069848007471551618267237710886541403235442402972757841211601305512508765941610080588540585990221962870251502405139968332129617187uwb,
				       (unsigned _BitInt(575)) 23115097167410578660280364356726488934202319035287675841474505237120386080336714513506555994051218240174733511200356850940493563860321836251689717522161899716121398557567847uwb)
	       == 327929344718748401627780651081744473244583534757535979608430300553738652522639530891741739uwb);
static_assert (__builtin_bitcompressg ((unsigned _BitInt(575)) 23115097167410578660280364356726488934202319035287675841474505237120386080336714513506555994051218240174733511200356850940493563860321836251689717522161899716121398557567847uwb,
				       (unsigned _BitInt(575)) 118131541925402402155086157499115656285751034069848007471551618267237710886541403235442402972757841211601305512508765941610080588540585990221962870251502405139968332129617187uwb)
	       == 2909031437005614947964838829905837773096498383064307256216302161781782214762584910989359uwb);
#endif
#if __BITINT_MAXWIDTH__ >= 1027
static_assert (__builtin_bitcompressg ((unsigned _BitInt(1027)) 294473855873745241271551195901386950656729759273365562206638967800992654698032636902920911003782368139677793592762040049443703439321090202240002644548914408256861393744782128292349394935235968569736234857665376806506152875996067378466482077335451170878303723908011096841794981692284316910893612604487249472311uwb,
				       (unsigned _BitInt(1027)) 540548418560750924575664703872106986534265345396756980894364393357877823859721129540519729973560861792682263121263569358086814437568179760344168293628388053637218170474254474264275367843920704704472260655775742000715646811343262012513538736719387133499156091882497205971330308173485560590613388499248986333824uwb)
	       == 1345657303990754153143578283893576247665548280540247072591967983899466500545664996992558492135467363277229823156703874541619044608181549372125254374uwb);
static_assert (__builtin_bitcompressg ((unsigned _BitInt(1027)) 540548418560750924575664703872106986534265345396756980894364393357877823859721129540519729973560861792682263121263569358086814437568179760344168293628388053637218170474254474264275367843920704704472260655775742000715646811343262012513538736719387133499156091882497205971330308173485560590613388499248986333824uwb,
				       (unsigned _BitInt(1027)) 294473855873745241271551195901386950656729759273365562206638967800992654698032636902920911003782368139677793592762040049443703439321090202240002644548914408256861393744782128292349394935235968569736234857665376806506152875996067378466482077335451170878303723908011096841794981692284316910893612604487249472311uwb)
	       == 450453790383945335553643124189396588460957680877946449247954125046510191930910925667177435230534006187607866941914049655956462502374002973515990229312uwb);
#endif

[[gnu::noipa]] unsigned char
bitcompress8 (unsigned char x, unsigned char m)
{
  return __builtin_bitcompressg (x, m);
}

[[gnu::noipa]] unsigned short
bitcompress16 (unsigned short x, unsigned short m)
{
  return __builtin_bitcompressg (x, m);
}

[[gnu::noipa]] unsigned int
bitcompress32 (unsigned int x, unsigned int m)
{
  return __builtin_bitcompressg (x, m);
}

[[gnu::noipa]] unsigned long
bitcompressl (unsigned long x, unsigned long m)
{
  return __builtin_bitcompressg (x, m);
}

[[gnu::noipa]] unsigned long long
bitcompress64 (unsigned long long x, unsigned long long m)
{
  return __builtin_bitcompressg (x, m);
}

#ifdef __SIZEOF_INT128__
[[gnu::noipa]] unsigned __int128
bitcompress128 (unsigned __int128 x, unsigned __int128 m)
{
  return __builtin_bitcompressg (x, m);
}
#endif

#if __BITINT_MAXWIDTH__ >= 256
[[gnu::noipa]] unsigned _BitInt(256)
bitcompress256 (unsigned _BitInt(256) x, unsigned _BitInt(256) y)
{
  return __builtin_bitcompressg (x, y);
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
[[gnu::noipa]] unsigned _BitInt(575)
bitcompress575 (unsigned _BitInt(575) x, unsigned _BitInt(575) y)
{
  return __builtin_bitcompressg (x, y);
}
#endif

#if __BITINT_MAXWIDTH__ >= 1027
[[gnu::noipa]] unsigned _BitInt(1027)
bitcompress1027 (unsigned _BitInt(1027) x, unsigned _BitInt(1027) y)
{
  return __builtin_bitcompressg (x, y);
}
#endif

#ifdef __SIZEOF_INT128__
#define T unsigned __int128
#else
#define T unsigned long long
#endif

[[gnu::noipa]] T
bitcompress8w (T x, T m)
{
  return bitcompress8 (x, m);
}

[[gnu::noipa]] T
bitcompress16w (T x, T m)
{
  return bitcompress16 (x, m);
}

[[gnu::noipa]] T
bitcompress32w (T x, T m)
{
  return bitcompress32 (x, m);
}

[[gnu::noipa]] T
bitcompresslw (T x, T m)
{
  return bitcompressl (x, m);
}

[[gnu::noipa]] T
bitcompress64w (T x, T m)
{
  return bitcompress64 (x, m);
}

int
main ()
{
  struct { T (*fn) (T, T); unsigned int digits; } tests[] = {
    { bitcompress8w, __CHAR_BIT__ },
    { bitcompress16w, __CHAR_BIT__ * __SIZEOF_SHORT__ },
    { bitcompress32w, __CHAR_BIT__ * __SIZEOF_INT__ },
    { bitcompresslw, __CHAR_BIT__ * __SIZEOF_LONG__ },
    { bitcompress64w, __CHAR_BIT__ * __SIZEOF_LONG_LONG__ },
#ifdef __SIZEOF_INT128__
    { bitcompress128, __CHAR_BIT__ * __SIZEOF_INT128__ },
#endif
  };
  for (int i = 0; i < sizeof (tests) / sizeof (tests[0]); ++i)
    {
      if (tests[i].fn (0xff, 0xff) != 0xff)
	__builtin_abort ();
      if (tests[i].fn (0xff, 0x0f) != 0x0f)
	__builtin_abort ();
      if (tests[i].fn (0xff, 0xf0) != 0x0f)
	__builtin_abort ();
      if ((tests[i].digits % 2) == 0 && tests[i].digits <= 64)
	{
	  if (tests[i].fn (0xaaaaaaaaaaaaaaaaULL, 0xccccccccccccccccULL)
	      != ((0xaaaaaaaaaaaaaaaaULL & (~0ULL >> (64 - tests[i].digits))) >> (tests[i].digits / 2)))
	    __builtin_abort ();
	  if (tests[i].fn (~0ULL, 0xaaaaaaaaaaaaaaaaULL)
	      != ((~0ULL >> (64 - tests[i].digits)) >> (tests[i].digits / 2)))
	    __builtin_abort ();
	}
      if (tests[i].fn (0xd4, 0xbf) != 0x54)
	__builtin_abort ();
      if (tests[i].fn (0x66, 0x1d) != 0x02)
	__builtin_abort ();
      if (tests[i].digits < 16)
	continue;
      if (tests[i].fn (0xebd4U, 0x93bfU) != 0x05d4U)
	__builtin_abort ();
      if (tests[i].fn (0xa166U, 0x001dU) != 2U)
	__builtin_abort ();
      if (tests[i].digits < 32)
	continue;
      if (tests[i].fn (0x9daeebd4UL, 0xad8793bfUL) != 0xbf5d4UL)
	__builtin_abort ();
      if (tests[i].fn (0x979ba166UL, 0x54ca001dUL) != 0x3b2UL)
	__builtin_abort ();
      if (tests[i].digits < 64)
	continue;
      if (tests[i].fn (0x5bd30f069daeebd4ULL, 0x64f9a89bad8793bfULL) != 0x13522bf5d4ULL)
	__builtin_abort ();
      if (tests[i].fn (0xb5672499979ba166ULL, 0xd315782354ca001dULL) != 0x2ad0bb2ULL)
	__builtin_abort ();
    }
#if __BITINT_MAXWIDTH__ >= 256
  {
    const unsigned _BitInt(256) a = 2977712650221868789329368963712957747535884589227943441826207435705422199671uwb;
    const unsigned _BitInt(256) b = 71392704391611725846964173788815479408492634097466601526619781877369006838376uwb;
    if (bitcompress256 (a, b) != __builtin_bitcompressg (a, b)
	|| bitcompress256 (-1, -1) != -1
	|| bitcompress256 (0, 0) != 0
	|| bitcompress256 (b, a) != __builtin_bitcompressg (b, a))
      __builtin_abort ();
  }
#endif
#if __BITINT_MAXWIDTH__ >= 575
  {
    const unsigned _BitInt(575) a = 118131541925402402155086157499115656285751034069848007471551618267237710886541403235442402972757841211601305512508765941610080588540585990221962870251502405139968332129617187uwb;
    const unsigned _BitInt(575) b = 23115097167410578660280364356726488934202319035287675841474505237120386080336714513506555994051218240174733511200356850940493563860321836251689717522161899716121398557567847uwb;
    if (bitcompress575 (a, b) != __builtin_bitcompressg (a, b)
	|| bitcompress575 (-1, -1) != -1
	|| bitcompress575 (0, 0) != 0
	|| bitcompress575 (b, a) != __builtin_bitcompressg (b, a))
      __builtin_abort ();
  }
#endif
#if __BITINT_MAXWIDTH__ >= 1027
  {
    const unsigned _BitInt(1027) a = 294473855873745241271551195901386950656729759273365562206638967800992654698032636902920911003782368139677793592762040049443703439321090202240002644548914408256861393744782128292349394935235968569736234857665376806506152875996067378466482077335451170878303723908011096841794981692284316910893612604487249472311uwb;
    const unsigned _BitInt(1027) b = 540548418560750924575664703872106986534265345396756980894364393357877823859721129540519729973560861792682263121263569358086814437568179760344168293628388053637218170474254474264275367843920704704472260655775742000715646811343262012513538736719387133499156091882497205971330308173485560590613388499248986333824uwb;
    if (bitcompress1027 (a, b) != __builtin_bitcompressg (a, b)
	|| bitcompress1027 (-1, -1) != -1
	|| bitcompress1027 (0, 0) != 0
	|| bitcompress1027 (b, a) != __builtin_bitcompressg (b, a))
      __builtin_abort ();
  }
#endif
}
