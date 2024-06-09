/* Reduced from linux 5.3.11: drivers/net/wireless/ath/ath10k/usb.c  */

/* The original file has this licence header.  */

// SPDX-License-Identifier: ISC
/*
 * Copyright (c) 2007-2011 Atheros Communications Inc.
 * Copyright (c) 2011-2012,2017 Qualcomm Atheros, Inc.
 * Copyright (c) 2016-2017 Erik Stromdahl <erik.stromdahl@gmail.com>
 */

/* Adapted from include/linux/compiler_attributes.h.  */
#define __printf(a, b)                  __attribute__((__format__(printf, a, b)))

/* From drivers/net/wireless/ath/ath10k/core.h.  */

struct ath10k;

/* From drivers/net/wireless/ath/ath10k/debug.h.  */

enum ath10k_debug_mask {
	/* [...other values removed...]  */
	ATH10K_DBG_USB_BULK	= 0x00080000,
};

extern unsigned int ath10k_debug_mask;

__printf(3, 4) void __ath10k_dbg(struct ath10k *ar,
				 enum ath10k_debug_mask mask,
				 const char *fmt, ...);

static void ath10k_usb_hif_tx_sg(struct ath10k *ar)
{
  if (ath10k_debug_mask & ATH10K_DBG_USB_BULK)
    __ath10k_dbg(ar, ATH10K_DBG_USB_BULK, "usb bulk transmit failed: %d\n", 42);
}
