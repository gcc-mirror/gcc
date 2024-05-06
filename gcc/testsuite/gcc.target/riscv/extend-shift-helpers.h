#ifndef EXTEND_SHIFT_HELPERS_H
#define EXTEND_SHIFT_HELPERS_H

#define RT_EXT_CT_RSHIFT_N_AT(RTS,RT,CTS,CT,N,ATS,AT)			\
RTS RT									\
RTS##_##RT##_ext_##CTS##_##CT##_rshift_##N##_##ATS##_##AT(ATS AT v)	\
{									\
    return (CTS CT)(v >> N);						\
}

#define ULONG_EXT_USHORT_RSHIFT_N_ULONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(unsigned,long,unsigned,short,N,unsigned,long)

#define ULONG_EXT_UINT_RSHIFT_N_ULONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(unsigned,long,unsigned,int,N,unsigned,long)

#define UINT_EXT_USHORT_RSHIFT_N_UINT(N) \
	RT_EXT_CT_RSHIFT_N_AT(unsigned,int,unsigned,short,N,unsigned,int)

#define UINT_EXT_USHORT_RSHIFT_N_ULONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(unsigned,int,unsigned,short,N,unsigned,long)

#define ULONG_EXT_USHORT_RSHIFT_N_UINT(N) \
	RT_EXT_CT_RSHIFT_N_AT(unsigned,long,unsigned,short,N,unsigned,int)

#endif /* EXTEND_SHIFT_HELPERS_H */
