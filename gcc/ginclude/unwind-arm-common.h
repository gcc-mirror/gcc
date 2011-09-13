/* Header file for the ARM EABI and C6X unwinders
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011
   Free Software Foundation, Inc.
   Contributed by Paul Brook

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Language-independent unwinder header public defines.  This contains both
   ABI defined objects, and GNU support routines.  */

#ifndef UNWIND_ARM_COMMON_H
#define UNWIND_ARM_COMMON_H

#define __ARM_EABI_UNWINDER__ 1

#ifdef __cplusplus
extern "C" {
#endif
  typedef unsigned _Unwind_Word __attribute__((__mode__(__word__)));
  typedef signed _Unwind_Sword __attribute__((__mode__(__word__)));
  typedef unsigned _Unwind_Ptr __attribute__((__mode__(__pointer__)));
  typedef unsigned _Unwind_Internal_Ptr __attribute__((__mode__(__pointer__)));
  typedef _Unwind_Word _uw;
  typedef unsigned _uw64 __attribute__((mode(__DI__)));
  typedef unsigned _uw16 __attribute__((mode(__HI__)));
  typedef unsigned _uw8 __attribute__((mode(__QI__)));

  typedef enum
    {
      _URC_OK = 0,       /* operation completed successfully */
      _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
      _URC_END_OF_STACK = 5,
      _URC_HANDLER_FOUND = 6,
      _URC_INSTALL_CONTEXT = 7,
      _URC_CONTINUE_UNWIND = 8,
      _URC_FAILURE = 9   /* unspecified failure of some kind */
    }
  _Unwind_Reason_Code;

  typedef enum
    {
      _US_VIRTUAL_UNWIND_FRAME = 0,
      _US_UNWIND_FRAME_STARTING = 1,
      _US_UNWIND_FRAME_RESUME = 2,
      _US_ACTION_MASK = 3,
      _US_FORCE_UNWIND = 8,
      _US_END_OF_STACK = 16
    }
  _Unwind_State;

  /* Provided only for compatibility with existing code.  */
  typedef int _Unwind_Action;
#define _UA_SEARCH_PHASE	1
#define _UA_CLEANUP_PHASE	2
#define _UA_HANDLER_FRAME	4
#define _UA_FORCE_UNWIND	8
#define _UA_END_OF_STACK	16
#define _URC_NO_REASON 	_URC_OK

  typedef struct _Unwind_Control_Block _Unwind_Control_Block;
  typedef struct _Unwind_Context _Unwind_Context;
  typedef _uw _Unwind_EHT_Header;


  /* UCB: */

  struct _Unwind_Control_Block
    {
      char exception_class[8];
      void (*exception_cleanup)(_Unwind_Reason_Code, _Unwind_Control_Block *);
      /* Unwinder cache, private fields for the unwinder's use */
      struct
	{
	  _uw reserved1;  /* Forced unwind stop fn, 0 if not forced */
	  _uw reserved2;  /* Personality routine address */
	  _uw reserved3;  /* Saved callsite address */
	  _uw reserved4;  /* Forced unwind stop arg */
	  _uw reserved5;
	}
      unwinder_cache;
      /* Propagation barrier cache (valid after phase 1): */
      struct
	{
	  _uw sp;
	  _uw bitpattern[5];
	}
      barrier_cache;
      /* Cleanup cache (preserved over cleanup): */
      struct
	{
	  _uw bitpattern[4];
	}
      cleanup_cache;
      /* Pr cache (for pr's benefit): */
      struct
	{
	  _uw fnstart;			/* function start address */
	  _Unwind_EHT_Header *ehtp;	/* pointer to EHT entry header word */
	  _uw additional;		/* additional data */
	  _uw reserved1;
	}
      pr_cache;
      long long int :0;	/* Force alignment to 8-byte boundary */
    };

  /* Virtual Register Set*/

  typedef enum
    {
      _UVRSC_CORE = 0,      /* integer register */
      _UVRSC_VFP = 1,       /* vfp */
      _UVRSC_FPA = 2,       /* fpa */
      _UVRSC_WMMXD = 3,     /* Intel WMMX data register */
      _UVRSC_WMMXC = 4      /* Intel WMMX control register */
    }
  _Unwind_VRS_RegClass;

  typedef enum
    {
      _UVRSD_UINT32 = 0,
      _UVRSD_VFPX = 1,
      _UVRSD_FPAX = 2,
      _UVRSD_UINT64 = 3,
      _UVRSD_FLOAT = 4,
      _UVRSD_DOUBLE = 5
    }
  _Unwind_VRS_DataRepresentation;

  typedef enum
    {
      _UVRSR_OK = 0,
      _UVRSR_NOT_IMPLEMENTED = 1,
      _UVRSR_FAILED = 2
    }
  _Unwind_VRS_Result;

  /* Frame unwinding state.  */
  typedef struct
    {
      /* The current word (bytes packed msb first).  */
      _uw data;
      /* Pointer to the next word of data.  */
      _uw *next;
      /* The number of bytes left in this word.  */
      _uw8 bytes_left;
      /* The number of words pointed to by ptr.  */
      _uw8 words_left;
    }
  __gnu_unwind_state;

  typedef _Unwind_Reason_Code (*personality_routine) (_Unwind_State,
      _Unwind_Control_Block *, _Unwind_Context *);

  _Unwind_VRS_Result _Unwind_VRS_Set(_Unwind_Context *, _Unwind_VRS_RegClass,
                                     _uw, _Unwind_VRS_DataRepresentation,
                                     void *);

  _Unwind_VRS_Result _Unwind_VRS_Get(_Unwind_Context *, _Unwind_VRS_RegClass,
                                     _uw, _Unwind_VRS_DataRepresentation,
                                     void *);

  _Unwind_VRS_Result _Unwind_VRS_Pop(_Unwind_Context *, _Unwind_VRS_RegClass,
                                     _uw, _Unwind_VRS_DataRepresentation);


  /* Support functions for the PR.  */
#define _Unwind_Exception _Unwind_Control_Block
  typedef char _Unwind_Exception_Class[8];

  void * _Unwind_GetLanguageSpecificData (_Unwind_Context *);
  _Unwind_Ptr _Unwind_GetRegionStart (_Unwind_Context *);

  _Unwind_Ptr _Unwind_GetDataRelBase (_Unwind_Context *);
  /* This should never be used.  */
  _Unwind_Ptr _Unwind_GetTextRelBase (_Unwind_Context *);

  /* Interface functions: */
  _Unwind_Reason_Code _Unwind_RaiseException(_Unwind_Control_Block *ucbp);
  void __attribute__((noreturn)) _Unwind_Resume(_Unwind_Control_Block *ucbp);
  _Unwind_Reason_Code _Unwind_Resume_or_Rethrow (_Unwind_Control_Block *ucbp);

  typedef _Unwind_Reason_Code (*_Unwind_Stop_Fn)
       (int, _Unwind_Action, _Unwind_Exception_Class,
	_Unwind_Control_Block *, struct _Unwind_Context *, void *);
  _Unwind_Reason_Code _Unwind_ForcedUnwind (_Unwind_Control_Block *,
					    _Unwind_Stop_Fn, void *);
  /* @@@ Use unwind data to perform a stack backtrace.  The trace callback
     is called for every stack frame in the call chain, but no cleanup
     actions are performed.  */
  typedef _Unwind_Reason_Code (*_Unwind_Trace_Fn) (_Unwind_Context *, void *);
  _Unwind_Reason_Code _Unwind_Backtrace(_Unwind_Trace_Fn,
					void*);

  _Unwind_Word _Unwind_GetCFA (struct _Unwind_Context *);
  void _Unwind_Complete(_Unwind_Control_Block *ucbp);
  void _Unwind_DeleteException (_Unwind_Exception *);

  _Unwind_Reason_Code __gnu_unwind_frame (_Unwind_Control_Block *,
					  _Unwind_Context *);
  _Unwind_Reason_Code __gnu_unwind_execute (_Unwind_Context *,
					    __gnu_unwind_state *);

  static inline _Unwind_Word
  _Unwind_GetGR (_Unwind_Context *context, int regno)
    {
      _uw val;
      _Unwind_VRS_Get (context, _UVRSC_CORE, regno, _UVRSD_UINT32, &val);
      return val;
    }

#define _Unwind_GetIPInfo(context, ip_before_insn) \
  (*ip_before_insn = 0, _Unwind_GetIP (context))

  static inline void
  _Unwind_SetGR (_Unwind_Context *context, int regno, _Unwind_Word val)
    {
      _Unwind_VRS_Set (context, _UVRSC_CORE, regno, _UVRSD_UINT32, &val);
    }

  _Unwind_Ptr _Unwind_GetRegionStart (_Unwind_Context *);
  void * _Unwind_GetLanguageSpecificData (_Unwind_Context *);

/* leb128 type numbers have a potentially unlimited size.
   The target of the following definitions of _sleb128_t and _uleb128_t
   is to have efficient data types large enough to hold the leb128 type
   numbers used in the unwind code.  */
typedef long _sleb128_t;
typedef unsigned long _uleb128_t;

#ifdef __cplusplus
}   /* extern "C" */
#endif

#endif /* defined UNWIND_ARM_COMMON_H */
