#ifndef EXTEND_SHIFT_HELPERS_H
#define EXTEND_SHIFT_HELPERS_H

#define RT_EXT_CT_RSHIFT_N_AT(RTS,RT,CTS,CT,N,ATS,AT)			\
RTS RT									\
RTS##_##RT##_ext_##CTS##_##CT##_rshift_##N##_##ATS##_##AT(ATS AT v)	\
{									\
    return (CTS CT)(v >> N);						\
}

#define SLONG_EXT_SCHAR_RSHIFT_N_SLONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(signed,long,signed,char,N,signed,long)

#define SLONG_EXT_SSHORT_RSHIFT_N_SLONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(signed,long,signed,short,N,signed,long)

#define SLONG_EXT_SINT_RSHIFT_N_SLONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(signed,long,signed,int,N,signed,long)

#define SINT_EXT_SSHORT_RSHIFT_N_SINT(N) \
	RT_EXT_CT_RSHIFT_N_AT(signed,int,signed,short,N,signed,int)

#define SINT_EXT_SSHORT_RSHIFT_N_SLONG(N) \
	RT_EXT_CT_RSHIFT_N_AT(signed,int,signed,short,N,signed,long)

#define SLONG_EXT_SSHORT_RSHIFT_N_SINT(N) \
	RT_EXT_CT_RSHIFT_N_AT(signed,long,signed,short,N,signed,int)



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
