/* stp.h -- Private #include File (module.h template V1.0)
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Owning Modules:
      stp.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_stp
#define _H_f_stp

/* Simple definitions and enumerations. */

enum _ffestp_acceptix_
  {
    FFESTP_acceptixFORMAT,
    FFESTP_acceptix
  };
typedef enum _ffestp_acceptix_ ffestpAcceptIx;

enum _ffestp_attrib_
  {
#if FFESTR_F90
    FFESTP_attribALLOCATABLE,
#endif
    FFESTP_attribDIMENSION,
    FFESTP_attribEXTERNAL,
#if FFESTR_F90
    FFESTP_attribINTENT,
#endif
    FFESTP_attribINTRINSIC,
#if FFESTR_F90
    FFESTP_attribOPTIONAL,
#endif
    FFESTP_attribPARAMETER,
#if FFESTR_F90
    FFESTP_attribPOINTER,
#endif
#if FFESTR_F90
    FFESTP_attribPRIVATE,
    FFESTP_attribPUBLIC,
#endif
    FFESTP_attribSAVE,
#if FFESTR_F90
    FFESTP_attribTARGET,
#endif
    FFESTP_attrib
  };
typedef enum _ffestp_attrib_ ffestpAttrib;

enum _ffestp_beruix_
  {
    FFESTP_beruixERR,
    FFESTP_beruixIOSTAT,
    FFESTP_beruixUNIT,
    FFESTP_beruix
  };
typedef enum _ffestp_beruix_ ffestpBeruIx;

enum _ffestp_closeix_
  {
    FFESTP_closeixERR,
    FFESTP_closeixIOSTAT,
    FFESTP_closeixSTATUS,
    FFESTP_closeixUNIT,
    FFESTP_closeix
  };
typedef enum _ffestp_closeix_ ffestpCloseIx;

enum _ffestp_deleteix_
  {
    FFESTP_deleteixERR,
    FFESTP_deleteixIOSTAT,
    FFESTP_deleteixREC,
    FFESTP_deleteixUNIT,
    FFESTP_deleteix
  };
typedef enum _ffestp_deleteix_ ffestpDeleteIx;

enum _ffestp_findix_
  {
    FFESTP_findixERR,
    FFESTP_findixIOSTAT,
    FFESTP_findixREC,
    FFESTP_findixUNIT,
    FFESTP_findix
  };
typedef enum _ffestp_findix_ ffestpFindIx;

enum _ffestp_inquireix_
  {
    FFESTP_inquireixACCESS,
    FFESTP_inquireixACTION,
    FFESTP_inquireixBLANK,
    FFESTP_inquireixCARRIAGECONTROL,
    FFESTP_inquireixDEFAULTFILE,
    FFESTP_inquireixDELIM,
    FFESTP_inquireixDIRECT,
    FFESTP_inquireixERR,
    FFESTP_inquireixEXIST,
    FFESTP_inquireixFILE,
    FFESTP_inquireixFORM,
    FFESTP_inquireixFORMATTED,
    FFESTP_inquireixIOLENGTH,
    FFESTP_inquireixIOSTAT,
    FFESTP_inquireixKEYED,
    FFESTP_inquireixNAME,
    FFESTP_inquireixNAMED,
    FFESTP_inquireixNEXTREC,
    FFESTP_inquireixNUMBER,
    FFESTP_inquireixOPENED,
    FFESTP_inquireixORGANIZATION,
    FFESTP_inquireixPAD,
    FFESTP_inquireixPOSITION,
    FFESTP_inquireixREAD,
    FFESTP_inquireixREADWRITE,
    FFESTP_inquireixRECL,
    FFESTP_inquireixRECORDTYPE,
    FFESTP_inquireixSEQUENTIAL,
    FFESTP_inquireixUNFORMATTED,
    FFESTP_inquireixUNIT,
    FFESTP_inquireixWRITE,
    FFESTP_inquireix
  };
typedef enum _ffestp_inquireix_ ffestpInquireIx;

enum _ffestp_openix_
  {
    FFESTP_openixACCESS,
    FFESTP_openixACTION,
    FFESTP_openixASSOCIATEVARIABLE,
    FFESTP_openixBLANK,
    FFESTP_openixBLOCKSIZE,
    FFESTP_openixBUFFERCOUNT,
    FFESTP_openixCARRIAGECONTROL,
    FFESTP_openixDEFAULTFILE,
    FFESTP_openixDELIM,
    FFESTP_openixDISPOSE,
    FFESTP_openixERR,
    FFESTP_openixEXTENDSIZE,
    FFESTP_openixFILE,
    FFESTP_openixFORM,
    FFESTP_openixINITIALSIZE,
    FFESTP_openixIOSTAT,
    FFESTP_openixKEY,
    FFESTP_openixMAXREC,
    FFESTP_openixNOSPANBLOCKS,
    FFESTP_openixORGANIZATION,
    FFESTP_openixPAD,
    FFESTP_openixPOSITION,
    FFESTP_openixREADONLY,
    FFESTP_openixRECL,
    FFESTP_openixRECORDTYPE,
    FFESTP_openixSHARED,
    FFESTP_openixSTATUS,
    FFESTP_openixUNIT,
    FFESTP_openixUSEROPEN,
    FFESTP_openix
  };
typedef enum _ffestp_openix_ ffestpOpenIx;

enum _ffestp_printix_
  {
    FFESTP_printixFORMAT,
    FFESTP_printix
  };
typedef enum _ffestp_printix_ ffestpPrintIx;

enum _ffestp_readix_
  {
    FFESTP_readixADVANCE,
    FFESTP_readixEND,
    FFESTP_readixEOR,
    FFESTP_readixERR,
    FFESTP_readixFORMAT,	/* Or NAMELIST (use expr info to
				   distinguish). */
    FFESTP_readixIOSTAT,
    FFESTP_readixKEYEQ,
    FFESTP_readixKEYGE,
    FFESTP_readixKEYGT,
    FFESTP_readixKEYID,
    FFESTP_readixNULLS,
    FFESTP_readixREC,
    FFESTP_readixSIZE,
    FFESTP_readixUNIT,
    FFESTP_readix
  };
typedef enum _ffestp_readix_ ffestpReadIx;

enum _ffestp_rewriteix_
  {
    FFESTP_rewriteixERR,
    FFESTP_rewriteixFMT,
    FFESTP_rewriteixIOSTAT,
    FFESTP_rewriteixUNIT,
    FFESTP_rewriteix
  };
typedef enum _ffestp_rewriteix_ ffestpRewriteIx;

enum _ffestp_typeix_
  {
    FFESTP_typeixFORMAT,
    FFESTP_typeix
  };
typedef enum _ffestp_typeix_ ffestpTypeIx;

enum _ffestp_vxtcodeix_
  {
    FFESTP_vxtcodeixB,
    FFESTP_vxtcodeixC,
    FFESTP_vxtcodeixERR,
    FFESTP_vxtcodeixF,
    FFESTP_vxtcodeixIOSTAT,
    FFESTP_vxtcodeix
  };
typedef enum _ffestp_vxtcodeix_ ffestpVxtcodeIx;

enum _ffestp_writeix_
  {
    FFESTP_writeixADVANCE,
    FFESTP_writeixEOR,
    FFESTP_writeixERR,
    FFESTP_writeixFORMAT,	/* Or NAMELIST (use expr info to
				   distinguish). */
    FFESTP_writeixIOSTAT,
    FFESTP_writeixREC,
    FFESTP_writeixUNIT,
    FFESTP_writeix
  };
typedef enum _ffestp_writeix_ ffestpWriteIx;

#if FFESTR_F90
enum _ffestp_definedoperator_
  {
    FFESTP_definedoperatorNone,	/* INTERFACE generic-name. */
    FFESTP_definedoperatorOPERATOR,	/* INTERFACE
					   OPERATOR(defined-operator). */
    FFESTP_definedoperatorASSIGNMENT,	/* INTERFACE ASSIGNMENT(=). */
    FFESTP_definedoperatorPOWER,
    FFESTP_definedoperatorMULT,
    FFESTP_definedoperatorADD,
    FFESTP_definedoperatorCONCAT,
    FFESTP_definedoperatorDIVIDE,
    FFESTP_definedoperatorSUBTRACT,
    FFESTP_definedoperatorNOT,
    FFESTP_definedoperatorAND,
    FFESTP_definedoperatorOR,
    FFESTP_definedoperatorEQV,
    FFESTP_definedoperatorNEQV,
    FFESTP_definedoperatorEQ,
    FFESTP_definedoperatorNE,
    FFESTP_definedoperatorLT,
    FFESTP_definedoperatorLE,
    FFESTP_definedoperatorGT,
    FFESTP_definedoperatorGE,
    FFESTP_definedoperator
  };
typedef enum _ffestp_definedoperator_ ffestpDefinedOperator;
#endif

enum _ffestp_dimtype_
  {
    FFESTP_dimtypeNONE,
    FFESTP_dimtypeKNOWN,	/* Known-bounds dimension list. */
    FFESTP_dimtypeADJUSTABLE,	/* Adjustable dimension list. */
    FFESTP_dimtypeASSUMED,	/* Assumed dimension list (known except for
				   last). */
    FFESTP_dimtypeADJUSTABLEASSUMED,	/* Both. */
    FFESTP_dimtype
  };
typedef enum _ffestp_dimtype_ ffestpDimtype;

enum _ffestp_formattype_
  {
    FFESTP_formattypeNone,
    FFESTP_formattypeI,
    FFESTP_formattypeB,
    FFESTP_formattypeO,
    FFESTP_formattypeZ,
    FFESTP_formattypeF,
    FFESTP_formattypeE,
    FFESTP_formattypeEN,
    FFESTP_formattypeG,
    FFESTP_formattypeL,
    FFESTP_formattypeA,
    FFESTP_formattypeD,
    FFESTP_formattypeQ,
    FFESTP_formattypeDOLLAR,	/* $ (V-extension). */
    FFESTP_formattypeP,
    FFESTP_formattypeT,
    FFESTP_formattypeTL,
    FFESTP_formattypeTR,
    FFESTP_formattypeX,
    FFESTP_formattypeS,
    FFESTP_formattypeSP,
    FFESTP_formattypeSS,
    FFESTP_formattypeBN,
    FFESTP_formattypeBZ,
    FFESTP_formattypeH,		/* Hollerith, used only for error-reporting. */
    FFESTP_formattypeSLASH,
    FFESTP_formattypeCOLON,
    FFESTP_formattypeR1016,	/* char-literal-constant or cHchars. */
    FFESTP_formattypeFORMAT,	/* [r](format-item-list). */
    FFESTP_formattype
  };
typedef enum _ffestp_formattype_ ffestpFormatType;

enum _ffestp_type_
  {
    FFESTP_typeNone,
    FFESTP_typeINTEGER,
    FFESTP_typeREAL,
    FFESTP_typeCOMPLEX,
    FFESTP_typeLOGICAL,
    FFESTP_typeCHARACTER,
    FFESTP_typeDBLPRCSN,
    FFESTP_typeDBLCMPLX,
    FFESTP_typeBYTE,
    FFESTP_typeWORD,
#if FFESTR_F90
    FFESTP_typeTYPE,
#endif
    FFESTP_type
  };
typedef enum _ffestp_type_ ffestpType;

/* Typedefs. */

typedef struct _ffest_accept_stmt_ ffestpAcceptStmt;
typedef struct _ffest_beru_stmt_ ffestpBeruStmt;
typedef struct _ffest_close_stmt_ ffestpCloseStmt;
typedef struct _ffest_delete_stmt_ ffestpDeleteStmt;
typedef struct _ffestp_file ffestpFile;
typedef struct _ffest_find_stmt_ ffestpFindStmt;
typedef struct _ffest_inquire_stmt_ ffestpInquireStmt;
typedef struct _ffest_open_stmt_ ffestpOpenStmt;
typedef struct _ffest_print_stmt_ ffestpPrintStmt;
typedef struct _ffest_read_stmt_ ffestpReadStmt;
typedef struct _ffest_rewrite_stmt_ ffestpRewriteStmt;
typedef struct _ffest_type_stmt_ ffestpTypeStmt;
typedef struct _ffest_vxtcode_stmt_ ffestpVxtcodeStmt;
typedef struct _ffest_write_stmt_ ffestpWriteStmt;

/* Include files needed by this one. */

#include "bld.h"
#include "lab.h"
#include "lex.h"
#include "stp.h"
#include "stt.h"

/* Structure definitions. */

struct _ffestp_file
  {
    bool kw_or_val_present;	/* If FALSE, all else is n/a. */
    bool kw_present;		/* Indicates whether kw has a token. */
    bool value_present;		/* Indicates whether value/expr are valid. */
    bool value_is_label;	/* TRUE if expr has no expression, value is
				   NUMBER. */
    ffelexToken kw;		/* The keyword, iff kw_or_val_present &&
				   kw_present. */
    ffelexToken value;		/* The value, iff kw_or_val_present &&
				   value_present. */
    union
      {
	ffebld expr;		/* The expr, iff kw_or_val_present &&
				   value_present && !value_is_label. */
	ffelab label;		/* The label, iff kw_or_val_present &&
				   value_present && value_is_label. */
      }
    u;
  };

struct _ffest_accept_stmt_
  {
    ffestpFile accept_spec[FFESTP_acceptix];
  };

struct _ffest_beru_stmt_
  {
    ffestpFile beru_spec[FFESTP_beruix];
  };

struct _ffest_close_stmt_
  {
    ffestpFile close_spec[FFESTP_closeix];
  };

struct _ffest_delete_stmt_
  {
    ffestpFile delete_spec[FFESTP_deleteix];
  };

struct _ffest_find_stmt_
  {
    ffestpFile find_spec[FFESTP_findix];
  };

struct _ffest_imp_list_
  {
    ffesttImpList next;
    ffesttImpList previous;
    ffelexToken first;
    ffelexToken last;		/* NULL if a single letter. */
  };

struct _ffest_inquire_stmt_
  {
    ffestpFile inquire_spec[FFESTP_inquireix];
  };

struct _ffest_open_stmt_
  {
    ffestpFile open_spec[FFESTP_openix];
  };

struct _ffest_print_stmt_
  {
    ffestpFile print_spec[FFESTP_printix];
  };

struct _ffest_read_stmt_
  {
    ffestpFile read_spec[FFESTP_readix];
  };

struct _ffest_rewrite_stmt_
  {
    ffestpFile rewrite_spec[FFESTP_rewriteix];
  };

struct _ffest_type_stmt_
  {
    ffestpFile type_spec[FFESTP_typeix];
  };

struct _ffest_vxtcode_stmt_
  {
    ffestpFile vxtcode_spec[FFESTP_vxtcodeix];
  };

struct _ffest_write_stmt_
  {
    ffestpFile write_spec[FFESTP_writeix];
  };

union _ffestp_fileu_
  {
    ffestpAcceptStmt accept;
    ffestpBeruStmt beru;
    ffestpCloseStmt close;
    ffestpDeleteStmt delete;
    ffestpFindStmt find;
    ffestpInquireStmt inquire;
    ffestpOpenStmt open;
    ffestpPrintStmt print;
    ffestpReadStmt read;
    ffestpRewriteStmt rewrite;
    ffestpTypeStmt type;
    ffestpVxtcodeStmt vxtcode;
    ffestpWriteStmt write;
  };

/* Global objects accessed by users of this module. */

extern union _ffestp_fileu_ ffestp_file;

/* Declare functions with prototypes. */


/* Define macros. */

#define ffestp_init_0()
#define ffestp_init_1()
#define ffestp_init_2()
#define ffestp_init_3()
#define ffestp_init_4()
#define ffestp_terminate_0()
#define ffestp_terminate_1()
#define ffestp_terminate_2()
#define ffestp_terminate_3()
#define ffestp_terminate_4()

/* End of #include file. */

#endif
