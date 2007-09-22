/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* C99 6.3 Conversions.

   Check if all conversions are ok.  */

#define CONV(TYPE, NAME) \
        float NAME ## _to_SF (TYPE a) { return a; } \
        float Sat ## NAME ## _to_SF (_Sat TYPE a) { return a; } \
        double NAME ## _to_DF (TYPE a) { return a; } \
        double Sat ## NAME ## _to_DF (_Sat TYPE a) { return a; } \
        TYPE SF_to_ ## NAME (float a) { return a; } \
        _Sat TYPE SF_to_Sat ## NAME (float a) { return a; } \
        TYPE DF_to_ ## NAME (double a) { return a; } \
        _Sat TYPE DF_to_Sat ## NAME (double a) { return a; } \
        signed char NAME ## _to_schar (TYPE a) { return a; } \
        signed char Sat ## NAME ## _to_schar (_Sat TYPE a) { return a; } \
        unsigned char NAME ## _to_uchar (TYPE a) { return a; } \
        unsigned char Sat ## NAME ## _to_uchar (_Sat TYPE a) { return a; } \
        short NAME ## _to_short (TYPE a) { return a; } \
        short _Sat ## NAME ## _to_short (_Sat TYPE a) { return a; } \
        unsigned short NAME ## _to_ushort (TYPE a) { return a; } \
        unsigned short _Sat ## NAME ## _to_ushort (_Sat TYPE a) { return a; } \
        int NAME ## _to_int (TYPE a) { return a; } \
        int _Sat ## NAME ## _to_int (_Sat TYPE a) { return a; } \
        unsigned int NAME ## _to_uint (TYPE a) { return a; } \
        unsigned int _Sat ## NAME ## _to_uint (_Sat TYPE a) { return a; } \
        long NAME ## _to_long (TYPE a) { return a; } \
        unsigned long NAME ## _to_ulong (TYPE a) { return a; } \
        long _Sat ## NAME ## _to_long (TYPE a) { return a; } \
        unsigned long _Sat ## NAME ## _to_ulong (TYPE a) { return a; } \
        long long NAME ## _to_longlong (TYPE a) { return a; } \
        long long _Sat ## NAME ## _to_longlong (TYPE a) { return a; } \
        unsigned long long NAME ## _to_ulonglong (TYPE a) { return a; } \
        unsigned long long _Sat ## NAME ## _to_ulonglong (TYPE a) { return a; } \
        TYPE schar_to_ ## NAME (signed char a) { return a; } \
        _Sat TYPE schar_to_Sat ## NAME (signed char a) { return a; } \
        TYPE uchar_to_ ## NAME (unsigned char a) { return a; } \
        _Sat TYPE uchar_to_Sat ## NAME (unsigned char a) { return a; } \
        TYPE short_to_ ## NAME (short a) { return a; } \
        TYPE ushort_to_ ## NAME (unsigned short a) { return a; } \
        TYPE int_to_ ## NAME (int a) { return a; } \
        TYPE uint_to_ ## NAME (unsigned int a) { return a; } \
        TYPE long_to_ ## NAME (long a) { return a; } \
        TYPE ulong_to_ ## NAME (unsigned long a) { return a; } \
        TYPE longlong_to_ ## NAME (long long a) { return a; } \
        TYPE ulonglong_to_ ## NAME (unsigned long long a) { return a; } \
        _Sat TYPE short_to_Sat ## NAME (short a) { return a; } \
        _Sat TYPE ushort_to_Sat ## NAME (unsigned short a) { return a; } \
        _Sat TYPE int_to_Sat ## NAME (int a) { return a; } \
        _Sat TYPE uint_to_Sat ## NAME (unsigned int a) { return a; } \
        _Sat TYPE long_to_Sat ## NAME (long a) { return a; } \
        _Sat TYPE ulong_to_Sat ## NAME (unsigned long a) { return a; } \
        _Sat TYPE longlong_to_Sat ## NAME (long long a) { return a; } \
        _Sat TYPE ulonglong_to_Sat ## NAME (unsigned long long a) { return a; } \
	TYPE SFR_to ## NAME (short _Fract a) { return a; } \
	TYPE FR_to ## NAME (_Fract a) { return a; } \
	TYPE LFR_to ## NAME (long _Fract a) { return a; } \
	TYPE LLFR_to ## NAME (long long _Fract a) { return a; } \
	TYPE USFR_to ## NAME (unsigned short _Fract a) { return a; } \
	TYPE UFR_to ## NAME (unsigned _Fract a) { return a; } \
	TYPE ULFR_to ## NAME (unsigned long _Fract a) { return a; } \
	TYPE ULLFR_to ## NAME (unsigned long long _Fract a) { return a; } \
	TYPE SAC_to ## NAME (short _Accum a) { return a; } \
	TYPE AC_to ## NAME (_Accum a) { return a; } \
	TYPE LAC_to ## NAME (long _Accum a) { return a; } \
	TYPE LLAC_to ## NAME (long long _Accum a) { return a; } \
	TYPE USAC_to ## NAME (unsigned short _Accum a) { return a; } \
	TYPE UAC_to ## NAME (unsigned _Accum a) { return a; } \
	TYPE ULAC_to ## NAME (unsigned long _Accum a) { return a; } \
	TYPE ULLAC_to ## NAME (unsigned long long _Accum a) { return a; } \
	TYPE SATSFR_to ## NAME (_Sat short _Fract a) { return a; } \
	TYPE SATFR_to ## NAME (_Sat _Fract a) { return a; } \
	TYPE SATLFR_to ## NAME (_Sat long _Fract a) { return a; } \
	TYPE SATLLFR_to ## NAME (_Sat long long _Fract a) { return a; } \
	TYPE SATUSFR_to ## NAME (_Sat unsigned short _Fract a) { return a; } \
	TYPE SATUFR_to ## NAME (_Sat unsigned _Fract a) { return a; } \
	TYPE SATULFR_to ## NAME (_Sat unsigned long _Fract a) { return a; } \
	TYPE SATULLFR_to ## NAME (_Sat unsigned long long _Fract a) { return a; } \
	TYPE SATSAC_to ## NAME (_Sat short _Accum a) { return a; } \
	TYPE SATAC_to ## NAME (_Sat _Accum a) { return a; } \
	TYPE SATLAC_to ## NAME (_Sat long _Accum a) { return a; } \
	TYPE SATLLAC_to ## NAME (_Sat long long _Accum a) { return a; } \
	TYPE SATUSAC_to ## NAME (_Sat unsigned short _Accum a) { return a; } \
	TYPE SATUAC_to ## NAME (_Sat unsigned _Accum a) { return a; } \
	TYPE SATULAC_to ## NAME (_Sat unsigned long _Accum a) { return a; } \
	TYPE SATULLAC_to ## NAME (_Sat unsigned long long _Accum a) { return a; } \
	_Sat TYPE SFR_to_SAT ## NAME (short _Fract a) { return a; } \
	_Sat TYPE FR_to_SAT ## NAME (_Fract a) { return a; } \
	_Sat TYPE LFR_to_SAT ## NAME (long _Fract a) { return a; } \
	_Sat TYPE LLFR_to_SAT ## NAME (long long _Fract a) { return a; } \
	_Sat TYPE USFR_to_SAT ## NAME (unsigned short _Fract a) { return a; } \
	_Sat TYPE UFR_to_SAT ## NAME (unsigned _Fract a) { return a; } \
	_Sat TYPE ULFR_to_SAT ## NAME (unsigned long _Fract a) { return a; } \
	_Sat TYPE ULLFR_to_SAT ## NAME (unsigned long long _Fract a) { return a; } \
	_Sat TYPE SAC_to_SAT ## NAME (short _Accum a) { return a; } \
	_Sat TYPE AC_to_SAT ## NAME (_Accum a) { return a; } \
	_Sat TYPE LAC_to_SAT ## NAME (long _Accum a) { return a; } \
	_Sat TYPE LLAC_to_SAT ## NAME (long long _Accum a) { return a; } \
	_Sat TYPE USAC_to_SAT ## NAME (unsigned short _Accum a) { return a; } \
	_Sat TYPE UAC_to_SAT ## NAME (unsigned _Accum a) { return a; } \
	_Sat TYPE ULAC_to_SAT ## NAME (unsigned long _Accum a) { return a; } \
	_Sat TYPE ULLAC_to_SAT ## NAME (unsigned long long _Accum a) { return a; } \
	_Sat TYPE SATSFR_to_SAT ## NAME (_Sat short _Fract a) { return a; } \
	_Sat TYPE SATFR_to_SAT ## NAME (_Sat _Fract a) { return a; } \
	_Sat TYPE SATLFR_to_SAT ## NAME (_Sat long _Fract a) { return a; } \
	_Sat TYPE SATLLFR_to_SAT ## NAME (_Sat long long _Fract a) { return a; } \
	_Sat TYPE SATUSFR_to_SAT ## NAME (_Sat unsigned short _Fract a) { return a; } \
	_Sat TYPE SATUFR_to_SAT ## NAME (_Sat unsigned _Fract a) { return a; } \
	_Sat TYPE SATULFR_to_SAT ## NAME (_Sat unsigned long _Fract a) { return a; } \
	_Sat TYPE SATULLFR_to_SAT ## NAME (_Sat unsigned long long _Fract a) { return a; } \
	_Sat TYPE SATSAC_to_SAT ## NAME (_Sat short _Accum a) { return a; } \
	_Sat TYPE SATAC_to_SAT ## NAME (_Sat _Accum a) { return a; } \
	_Sat TYPE SATLAC_to_SAT ## NAME (_Sat long _Accum a) { return a; } \
	_Sat TYPE SATLLAC_to_SAT ## NAME (_Sat long long _Accum a) { return a; } \
	_Sat TYPE SATUSAC_to_SAT ## NAME (_Sat unsigned short _Accum a) { return a; } \
	_Sat TYPE SATUAC_to_SAT ## NAME (_Sat unsigned _Accum a) { return a; } \
	_Sat TYPE SATULAC_to_SAT ## NAME (_Sat unsigned long _Accum a) { return a; } \
	_Sat TYPE SATULLAC_to_SAT ## NAME (_Sat unsigned long long _Accum a) { return a; } \
	_Complex int NAME ## _to_CI (TYPE a) { return a; } \
	_Complex unsigned int NAME ## _to_CUI (TYPE a) { return a; } \
	_Complex float NAME ## _to_CF (TYPE a) { return a; } \
	_Complex double NAME ## _to_CD (TYPE a) { return a; } \
	TYPE CI_to_ ## NAME (_Complex int a) { return a; } \
	TYPE CUI_to_ ## NAME (_Complex unsigned int a) { return a; } \
	TYPE CF_to_ ## NAME (_Complex float a) { return a; } \
	TYPE CD_to_ ## NAME (_Complex double a) { return a; } \
	_Sat TYPE CI_to_SAT ## NAME (_Complex int a) { return a; } \
	_Sat TYPE CUI_to_SAT ## NAME (_Complex unsigned int a) { return a; } \
	_Sat TYPE CF_to_SAT ## NAME (_Complex float a) { return a; } \
	_Sat TYPE CD_to_SAT ## NAME (_Complex double a) { return a; }

CONV(short _Fract, sf);
CONV(_Fract, f);
CONV(long _Fract, lf);
CONV(long long _Fract, llf);
CONV(unsigned short _Fract, usf);
CONV(unsigned _Fract, uf);
CONV(unsigned long _Fract, ulf);
CONV(unsigned long long _Fract, ullf);

CONV(short _Accum, sk);
CONV(_Accum, k);
CONV(long _Accum, lk);
CONV(long long _Accum, llk);
CONV(unsigned short _Accum, usk);
CONV(unsigned _Accum, uk);
CONV(unsigned long _Accum, ulk);
CONV(unsigned long long _Accum, ullk);
