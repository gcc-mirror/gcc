/* Copyright (C) 2006, 2008, 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef __SPU_MFCIO_H__
#define __SPU_MFCIO_H__ 1

#include <spu_intrinsics.h>
#ifdef __IN_LIBGCC2
typedef unsigned long long uint64_t;
#else
#include <stdint.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif


/****************************************************************/
/* DMA list element structure*/
/****************************************************************/
 
#ifdef __GNUC__
__extension__
#endif
typedef struct mfc_list_element {
  uint64_t notify       :  1;   /** Stall-and-notify bit  */
  uint64_t reserved     : 16;
  uint64_t size         : 15;   /** Transfer size */
  uint64_t eal          : 32;   /** Lower word of effective address */
} mfc_list_element_t;
 
/****************************************************************/
/* DMA max/min size definitions.                        */
/****************************************************************/

#define MFC_MIN_DMA_SIZE_SHIFT  4      /* 16 bytes */
#define MFC_MAX_DMA_SIZE_SHIFT 14      /* 16384 bytes */

#define MFC_MIN_DMA_SIZE (1 << MFC_MIN_DMA_SIZE_SHIFT)
#define MFC_MAX_DMA_SIZE (1 << MFC_MAX_DMA_SIZE_SHIFT)

#define MFC_MIN_DMA_SIZE_MASK (MFC_MIN_DMA_SIZE - 1)
#define MFC_MAX_DMA_SIZE_MASK (MFC_MAX_DMA_SIZE - 1)

#define MFC_MIN_DMA_LIST_ELEMENTS 1
#define MFC_MAX_DMA_LIST_ELEMENTS 2048

#define MFC_MIN_DMA_LIST_SIZE (MFC_MIN_DMA_LIST_ELEMENTS << 3) /*   8 bytes */
#define MFC_MAX_DMA_LIST_SIZE (MFC_MAX_DMA_LIST_ELEMENTS << 3) /* 16K bytes */

/****************************************************************/
/* MFC DMA command modifiers to identify classes of operations. */
/****************************************************************/

/* Note: These commands modifier may be used in conjunction with the base
   command types (i.e. MFC_PUT_CMD, MFC_GET_CMD, and MFC_SNDSIG_CMD)
   to construct the various command permutations.  */

#define MFC_BARRIER_ENABLE    0x0001
#define MFC_FENCE_ENABLE      0x0002
#define MFC_LIST_ENABLE       0x0004
#define MFC_RESULT_ENABLE     0x0010

/****************************************************************/
/* MFC DMA Put Commands                                 */
/****************************************************************/

#define MFC_PUT_CMD          0x0020
#define MFC_PUTB_CMD         (MFC_PUT_CMD | MFC_BARRIER_ENABLE)
#define MFC_PUTF_CMD         (MFC_PUT_CMD | MFC_FENCE_ENABLE)
#define MFC_PUTL_CMD         (MFC_PUT_CMD | MFC_LIST_ENABLE)
#define MFC_PUTLB_CMD        (MFC_PUTL_CMD | MFC_BARRIER_ENABLE)
#define MFC_PUTLF_CMD        (MFC_PUTL_CMD | MFC_FENCE_ENABLE)

#define MFC_PUTR_CMD         (MFC_PUT_CMD | MFC_RESULT_ENABLE)
#define MFC_PUTRB_CMD        (MFC_PUTR_CMD | MFC_BARRIER_ENABLE)
#define MFC_PUTRF_CMD        (MFC_PUTR_CMD | MFC_FENCE_ENABLE)
#define MFC_PUTRL_CMD        (MFC_PUTR_CMD | MFC_LIST_ENABLE)
#define MFC_PUTRLB_CMD       (MFC_PUTRL_CMD | MFC_BARRIER_ENABLE)
#define MFC_PUTRLF_CMD       (MFC_PUTRL_CMD | MFC_FENCE_ENABLE)

/****************************************************************/
/* MFC DMA Get Commands                                 */
/****************************************************************/

#define MFC_GET_CMD          0x0040
#define MFC_GETB_CMD         (MFC_GET_CMD | MFC_BARRIER_ENABLE)
#define MFC_GETF_CMD         (MFC_GET_CMD | MFC_FENCE_ENABLE)
#define MFC_GETL_CMD         (MFC_GET_CMD | MFC_LIST_ENABLE)
#define MFC_GETLB_CMD        (MFC_GETL_CMD | MFC_BARRIER_ENABLE)
#define MFC_GETLF_CMD        (MFC_GETL_CMD | MFC_FENCE_ENABLE)

/****************************************************************/
/* MFC Synchronization Commands                           */
/****************************************************************/

#define MFC_SNDSIG_CMD       0x00A0
#define MFC_SNDSIGB_CMD      (MFC_SNDSIG_CMD | MFC_BARRIER_ENABLE)
#define MFC_SNDSIGF_CMD      (MFC_SNDSIG_CMD | MFC_FENCE_ENABLE)
#define MFC_BARRIER_CMD      0x00C0
#define MFC_EIEIO_CMD        0x00C8
#define MFC_SYNC_CMD         0x00CC

/****************************************************************/
/* MFC Atomic Commands                                 */
/****************************************************************/

#define MFC_GETLLAR_CMD      0x00D0
#define MFC_PUTLLC_CMD       0x00B4
#define MFC_PUTLLUC_CMD      0x00B0
#define MFC_PUTQLLUC_CMD     0x00B8

/****************************************************************/
/* MFC SL1 Storage Control Commands                             */
/****************************************************************/

#define MFC_SDCRT_CMD        0x0080
#define MFC_SDCRTST_CMD      0x0081
#define MFC_SDCRZ_CMD        0x0089
#define MFC_SDCRST_CMD       0x008D
#define MFC_SDCRF_CMD        0x008F

/****************************************************************/
/* Channel Defines                                    */
/****************************************************************/

/* Events Defines for channels
 *    0 (SPU_RdEventStat),
 *    1 (SPU_WrEventMask), and
 *    2 (SPU_WrEventAck).
 */
#define MFC_TAG_STATUS_UPDATE_EVENT         0x00000001
#define MFC_LIST_STALL_NOTIFY_EVENT         0x00000002
#define MFC_COMMAND_QUEUE_AVAILABLE_EVENT   0x00000008
#define MFC_IN_MBOX_AVAILABLE_EVENT         0x00000010
#define MFC_DECREMENTER_EVENT               0x00000020
#define MFC_OUT_INTR_MBOX_AVAILABLE_EVENT   0x00000040
#define MFC_OUT_MBOX_AVAILABLE_EVENT        0x00000080
#define MFC_SIGNAL_NOTIFY_2_EVENT           0x00000100
#define MFC_SIGNAL_NOTIFY_1_EVENT           0x00000200
#define MFC_LLR_LOST_EVENT                  0x00000400
#define MFC_PRIV_ATTN_EVENT                 0x00000800
#define MFC_MULTI_SRC_SYNC_EVENT            0x00001000

/* Tag Status Update defines for channel 23 (MFC_WrTagUpdate) */
#define MFC_TAG_UPDATE_IMMEDIATE   0x0
#define MFC_TAG_UPDATE_ANY         0x1
#define MFC_TAG_UPDATE_ALL         0x2

/* Atomic Command Status defines for channel 27 (MFC_RdAtomicStat) */
#define MFC_PUTLLC_STATUS    0x00000001
#define MFC_PUTLLUC_STATUS   0x00000002
#define MFC_GETLLAR_STATUS   0x00000004


/****************************************************************/
/* Definitions for constructing a 32-bit command word         */
/* including the transfer and replacement class id and the      */
/* command opcode.                                    */
/****************************************************************/
#define MFC_CMD_WORD(_tid, _rid, _cmd) (((_tid)<<24)|((_rid)<<16)|(_cmd))


/* Addressing Utilities */
#define mfc_ea2h(ea)   (unsigned int)((unsigned long long)(ea)>>32)
#define mfc_ea2l(ea)   (unsigned int)(ea)
#define mfc_hl2ea(h,l)   si_to_ullong(si_selb(si_from_uint(h),\
                                  si_rotqbyi(si_from_uint(l), -4),\
                                  si_fsmbi(0x0f0f)))
#define mfc_ceil128(v)   (((v) + 127) & ~127)

/* MFC DMA */
#define mfc_put(  ls,ea,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_PUT_CMD))
#define mfc_putf( ls,ea,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_PUTF_CMD))
#define mfc_putb( ls,ea,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_PUTB_CMD))
#define mfc_get(  ls,ea,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_GET_CMD))
#define mfc_getf( ls,ea,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_GETF_CMD))
#define mfc_getb( ls,ea,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_GETB_CMD))

/* MFC list DMA */
#define mfc_putl(  ls,ea,lsa,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),(unsigned int)(lsa),size,tag,MFC_CMD_WORD(tid,rid,MFC_PUTL_CMD))
#define mfc_putlf( ls,ea,lsa,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),(unsigned int)(lsa),size,tag,MFC_CMD_WORD(tid,rid,MFC_PUTLF_CMD))
#define mfc_putlb( ls,ea,lsa,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),(unsigned int)(lsa),size,tag,MFC_CMD_WORD(tid,rid,MFC_PUTLB_CMD))
#define mfc_getl(  ls,ea,lsa,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),(unsigned int)(lsa),size,tag,MFC_CMD_WORD(tid,rid,MFC_GETL_CMD))
#define mfc_getlf( ls,ea,lsa,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),(unsigned int)(lsa),size,tag,MFC_CMD_WORD(tid,rid,MFC_GETLF_CMD))
#define mfc_getlb( ls,ea,lsa,size,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),(unsigned int)(lsa),size,tag,MFC_CMD_WORD(tid,rid,MFC_GETLB_CMD))

/* MFC Atomic Update DMA */
#define mfc_getllar( ls,ea,tid,rid)     spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),128,  0,MFC_CMD_WORD(tid,rid,MFC_GETLLAR_CMD))
#define mfc_putllc(  ls,ea,tid,rid)     spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),128,  0,MFC_CMD_WORD(tid,rid,MFC_PUTLLC_CMD))
#define mfc_putlluc( ls,ea,tid,rid)     spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),128,  0,MFC_CMD_WORD(tid,rid,MFC_PUTLLUC_CMD))
#define mfc_putqlluc(ls,ea,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),128,tag,MFC_CMD_WORD(tid,rid,MFC_PUTQLLUC_CMD))

/* MFC Synchronization Commands */
#define mfc_sndsig( ls,ea,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),4,tag,MFC_CMD_WORD(tid,rid,MFC_SNDSIG_CMD))
#define mfc_sndsigb(ls,ea,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),4,tag,MFC_CMD_WORD(tid,rid,MFC_SNDSIGB_CMD))
#define mfc_sndsigf(ls,ea,tag,tid,rid) spu_mfcdma64(ls,mfc_ea2h(ea),mfc_ea2l(ea),4,tag,MFC_CMD_WORD(tid,rid,MFC_SNDSIGF_CMD))
#define mfc_barrier(tag)       spu_mfcdma32(0,0,0,tag,MFC_BARRIER_CMD)
#define mfc_eieio(tag,tid,rid) spu_mfcdma32(0,0,0,tag,MFC_CMD_WORD(tid,rid,MFC_EIEIO_CMD))
#define mfc_sync(tag)          spu_mfcdma32(0,0,0,tag,MFC_SYNC_CMD)

/* MFC SL1 Storage Control Commands */
#define mfc_sdcrt(  ea,size,tag,tid,rid) spu_mfcdma64(0,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_SDCRT_CMD))
#define mfc_sdcrtst(ea,size,tag,tid,rid) spu_mfcdma64(0,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_SDCRTST_CMD))
#define mfc_sdcrz(  ea,size,tag,tid,rid) spu_mfcdma64(0,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_SDCRZ_CMD))
#define mfc_sdcrst( ea,size,tag,tid,rid) spu_mfcdma64(0,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_SDCRST_CMD))
#define mfc_sdcrf(  ea,size,tag,tid,rid) spu_mfcdma64(0,mfc_ea2h(ea),mfc_ea2l(ea),size,tag,MFC_CMD_WORD(tid,rid,MFC_SDCRF_CMD))

/* DMA Queue */
#define mfc_stat_cmd_queue()          spu_readchcnt(MFC_Cmd)

/* MFC Tag-Status */
#define mfc_write_tag_mask(mask)      spu_writech(MFC_WrTagMask,mask)
#define mfc_read_tag_mask()           spu_readch(MFC_RdTagMask)

#define mfc_write_tag_update(ts)         spu_writech(MFC_WrTagUpdate,ts)
#define mfc_write_tag_update_immediate() mfc_write_tag_update(MFC_TAG_UPDATE_IMMEDIATE)
#define mfc_write_tag_update_any()       mfc_write_tag_update(MFC_TAG_UPDATE_ANY)
#define mfc_write_tag_update_all()       mfc_write_tag_update(MFC_TAG_UPDATE_ALL)
#define mfc_stat_tag_update()            spu_readchcnt(MFC_WrTagUpdate)

#define mfc_read_tag_status()            spu_readch(MFC_RdTagStat)
#define mfc_read_tag_status_immediate()  (mfc_write_tag_update_immediate(), mfc_read_tag_status())
#define mfc_read_tag_status_any()        (mfc_write_tag_update_any(), mfc_read_tag_status())
#define mfc_read_tag_status_all()        (mfc_write_tag_update_all(), mfc_read_tag_status())
#define mfc_stat_tag_status()            spu_readchcnt(MFC_RdTagStat)

/* MFC List Stall-and-Notify Tag */
#define mfc_read_list_stall_status()     spu_readch(MFC_RdListStallStat)
#define mfc_stat_list_stall_status()     spu_readchcnt(MFC_RdListStallStat)
#define mfc_write_list_stall_ack(tag)    spu_writech(MFC_WrListStallAck,tag)

/* Atomic DMA */
#define mfc_read_atomic_status()      spu_readch(MFC_RdAtomicStat)
#define mfc_stat_atomic_status()      spu_readchcnt(MFC_RdAtomicStat)

/* MFC Multi-source Synchronization */
#define mfc_write_multi_src_sync_request()   spu_writech(MFC_WrMSSyncReq,0)
#define mfc_stat_multi_src_sync_request()    spu_readchcnt(MFC_WrMSSyncReq)

/* SPU Signal */
#define spu_read_signal1()            spu_readch(SPU_RdSigNotify1)
#define spu_stat_signal1()            spu_readchcnt(SPU_RdSigNotify1)
#define spu_read_signal2()            spu_readch(SPU_RdSigNotify2)
#define spu_stat_signal2()            spu_readchcnt(SPU_RdSigNotify2)

/* SPU/PPE Mailbox */
#define spu_read_in_mbox()            spu_readch(SPU_RdInMbox)
#define spu_stat_in_mbox()            spu_readchcnt(SPU_RdInMbox)
#define spu_write_out_mbox(a)         spu_writech(SPU_WrOutMbox,a)
#define spu_stat_out_mbox()           spu_readchcnt(SPU_WrOutMbox)
#define spu_write_out_intr_mbox(a)    spu_writech(SPU_WrOutIntrMbox,a)
#define spu_stat_out_intr_mbox()      spu_readchcnt(SPU_WrOutIntrMbox)

/* SPU Decrementer */
#define spu_read_decrementer()        spu_readch(SPU_RdDec)
#define spu_write_decrementer(cnt)    spu_writech(SPU_WrDec,(cnt))

/* SPU Event */
#define spu_read_event_status()       spu_readch(SPU_RdEventStat)
#define spu_stat_event_status()       spu_readchcnt(SPU_RdEventStat)
#define spu_write_event_mask(mask)    spu_writech(SPU_WrEventMask,(mask))
#define spu_write_event_ack(ack)      spu_writech(SPU_WrEventAck,(ack))
#define spu_read_event_mask()         spu_readch(SPU_RdEventMask)

/* SPU State Management */
#define spu_read_machine_status()     spu_readch(SPU_RdMachStat)
#define spu_write_srr0(srr0)          spu_writech(SPU_WrSRR0,srr0)
#define spu_read_srr0()               spu_readch(SPU_RdSRR0)

/* Interrupt-Safe Critical Sections */

static __inline__ unsigned int mfc_begin_critical_section (void)
  __attribute__ ((__always_inline__));

static __inline__ unsigned int
mfc_begin_critical_section (void)
{
#ifdef SPU_MFCIO_INTERRUPT_SAFE
  unsigned int __status = spu_read_machine_status ();
  spu_idisable ();
  return __status;
#else
  return 0;
#endif
}

static __inline__ void mfc_end_critical_section (unsigned int)
  __attribute__ ((__always_inline__));

static __inline__ void
mfc_end_critical_section (unsigned int __status __attribute__ ((__unused__)))
{
#ifdef SPU_MFCIO_INTERRUPT_SAFE
  if (__status & 1)
    spu_ienable ();
#endif
}

/* MFC Tag Manager */

#define MFC_TAG_INVALID 0xFFFFFFFF
#define MFC_TAG_VALID   0x00000000

#define mfc_tag_reserve() \
	__mfc_tag_reserve()
#define mfc_tag_release(tag) \
	__mfc_tag_release((tag))
#define mfc_multi_tag_reserve(nr_tags) \
	__mfc_multi_tag_reserve((nr_tags))
#define mfc_multi_tag_release(tag, nr_tags) \
	__mfc_multi_tag_release((tag),(nr_tags))

extern unsigned int __mfc_tag_reserve (void);
extern unsigned int __mfc_tag_release (unsigned int);
extern unsigned int __mfc_multi_tag_reserve (unsigned int);
extern unsigned int __mfc_multi_tag_release (unsigned int, unsigned int);

#ifdef __cplusplus
}
#endif

#endif /* __SPU_MFCIO_H__ */
