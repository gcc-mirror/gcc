/* Extracted from i386.c for compile time issues with ranger taking a
   long time to evalaute deeply nested logical expressions.  */
 
/*  { dg-do compile } */
/*  { dg-options "-O2" } */

enum machine_mode
{
  E_VOIDmode,
  E_BLKmode,
  E_CCmode,
  E_CCGCmode,
  E_CCGOCmode,
  E_CCNOmode,
  E_CCGZmode,
  E_CCAmode,
  E_CCCmode,
  E_CCOmode,
  E_CCPmode,
  E_CCSmode,
  E_CCZmode,
  E_CCFPmode,
  E_BImode,
  E_QImode,
  E_HImode,
  E_SImode,
  E_DImode,
  E_TImode,
  E_OImode,
  E_XImode,
  E_P2QImode,
  E_P2HImode,
  E_QQmode,
  E_HQmode,
  E_SQmode,
  E_DQmode,
  E_TQmode,
  E_UQQmode,
  E_UHQmode,
  E_USQmode,
  E_UDQmode,
  E_UTQmode,
  E_HAmode,
  E_SAmode,
  E_DAmode,
  E_TAmode,
  E_UHAmode,
  E_USAmode,
  E_UDAmode,
  E_UTAmode,
  E_SFmode,
  E_DFmode,
  E_XFmode,
  E_TFmode,
  E_SDmode,
  E_DDmode,
  E_TDmode,
  E_CQImode,
  E_CP2QImode,
  E_CHImode,
  E_CP2HImode,
  E_CSImode,
  E_CDImode,
  E_CTImode,
  E_COImode,
  E_CXImode,
  E_SCmode,
  E_DCmode,
  E_XCmode,
  E_TCmode,
  E_V2QImode,
  E_V4QImode,
  E_V2HImode,
  E_V1SImode,
  E_V8QImode,
  E_V4HImode,
  E_V2SImode,
  E_V1DImode,
  E_V12QImode,
  E_V6HImode,
  E_V14QImode,
  E_V16QImode,
  E_V8HImode,
  E_V4SImode,
  E_V2DImode,
  E_V1TImode,
  E_V32QImode,
  E_V16HImode,
  E_V8SImode,
  E_V4DImode,
  E_V2TImode,
  E_V64QImode,
  E_V32HImode,
  E_V16SImode,
  E_V8DImode,
  E_V4TImode,
  E_V128QImode,
  E_V64HImode,
  E_V32SImode,
  E_V16DImode,
  E_V8TImode,
  E_V64SImode,
  E_V2SFmode,
  E_V4SFmode,
  E_V2DFmode,
  E_V8SFmode,
  E_V4DFmode,
  E_V2TFmode,
  E_V16SFmode,
  E_V8DFmode,
  E_V4TFmode,
  E_V32SFmode,
  E_V16DFmode,
  E_V8TFmode,
  E_V64SFmode,
  E_V32DFmode,
  E_V16TFmode,
  MAX_MACHINE_MODE,
  MIN_MODE_RANDOM = E_VOIDmode,
  MAX_MODE_RANDOM = E_BLKmode,
  MIN_MODE_CC = E_CCmode,
  MAX_MODE_CC = E_CCFPmode,
  MIN_MODE_INT = E_QImode,
  MAX_MODE_INT = E_XImode,
  MIN_MODE_PARTIAL_INT = E_P2QImode,
  MAX_MODE_PARTIAL_INT = E_P2HImode,
  MIN_MODE_FRACT = E_QQmode,
  MAX_MODE_FRACT = E_TQmode,
  MIN_MODE_UFRACT = E_UQQmode,
  MAX_MODE_UFRACT = E_UTQmode,
  MIN_MODE_ACCUM = E_HAmode,
  MAX_MODE_ACCUM = E_TAmode,
  MIN_MODE_UACCUM = E_UHAmode,
  MAX_MODE_UACCUM = E_UTAmode,
  MIN_MODE_FLOAT = E_SFmode,
  MAX_MODE_FLOAT = E_TFmode,
  MIN_MODE_DECIMAL_FLOAT = E_SDmode,
  MAX_MODE_DECIMAL_FLOAT = E_TDmode,
  MIN_MODE_COMPLEX_INT = E_CQImode,
  MAX_MODE_COMPLEX_INT = E_CXImode,
  MIN_MODE_COMPLEX_FLOAT = E_SCmode,
  MAX_MODE_COMPLEX_FLOAT = E_TCmode,
  MIN_MODE_VECTOR_BOOL = E_VOIDmode,
  MAX_MODE_VECTOR_BOOL = E_VOIDmode,
  MIN_MODE_VECTOR_INT = E_V2QImode,
  MAX_MODE_VECTOR_INT = E_V64SImode,
  MIN_MODE_VECTOR_FRACT = E_VOIDmode,
  MAX_MODE_VECTOR_FRACT = E_VOIDmode,
  MIN_MODE_VECTOR_UFRACT = E_VOIDmode,
  MAX_MODE_VECTOR_UFRACT = E_VOIDmode,
  MIN_MODE_VECTOR_ACCUM = E_VOIDmode,
  MAX_MODE_VECTOR_ACCUM = E_VOIDmode,
  MIN_MODE_VECTOR_UACCUM = E_VOIDmode,
  MAX_MODE_VECTOR_UACCUM = E_VOIDmode,
  MIN_MODE_VECTOR_FLOAT = E_V2SFmode,
  MAX_MODE_VECTOR_FLOAT = E_V16TFmode,
  NUM_MACHINE_MODES = MAX_MACHINE_MODE
};

extern unsigned get_mode_alignment (enum machine_mode);
long
ix86_static_rtx_alignment (enum machine_mode mode)
{
  if (mode == E_DFmode)
    return 64;
  if (((mode) == E_XFmode
       || ((mode) == ((void) 0, E_V1TImode)
	   || (mode) == E_TImode
	   || (mode) == ((void) 0, E_V16QImode)
	   || (mode) == E_TFmode
	   || (mode) == ((void) 0, E_V8HImode)
	   || (mode) == ((void) 0, E_V2DFmode)
	   || (mode) == ((void) 0, E_V2DImode)
	   || (mode) == ((void) 0, E_V4SFmode)
	   || (mode) == ((void) 0, E_V4SImode)
	   || (mode) == ((void) 0, E_V32QImode)
	   || (mode) == ((void) 0, E_V16HImode)
	   || (mode) == ((void) 0, E_V8SImode)
	   || (mode) == ((void) 0, E_V4DImode)
	   || (mode) == ((void) 0, E_V8SFmode)
	   || (mode) == ((void) 0, E_V4DFmode)
	   || (mode) == ((void) 0, E_V2TImode)
	   || (mode) == ((void) 0, E_V8DImode)
	   || (mode) == ((void) 0, E_V64QImode)
	   || (mode) == ((void) 0, E_V16SImode)
	   || (mode) == ((void) 0, E_V32HImode)
	   || (mode) == ((void) 0, E_V8DFmode)
	   || (mode) == ((void) 0, E_V16SFmode))))
    return ((128) >
	    (get_mode_alignment (mode)) ? (128)
	    : (get_mode_alignment (mode)));
  return get_mode_alignment (mode);
}
