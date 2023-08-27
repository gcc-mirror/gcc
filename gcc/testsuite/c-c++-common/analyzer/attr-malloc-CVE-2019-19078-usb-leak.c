/* Adapted from linux 5.3.11: drivers/net/wireless/ath/ath10k/usb.c
   Reduced reproducer for CVE-2019-19078 (leak of struct urb).  */

typedef unsigned char u8;
typedef unsigned short u16;

#ifndef __cplusplus 
  typedef _Bool bool;
#endif

#define	ENOMEM		12
#define	EINVAL		22

/* The original file has this licence header.  */

// SPDX-License-Identifier: ISC
/*
 * Copyright (c) 2007-2011 Atheros Communications Inc.
 * Copyright (c) 2011-2012,2017 Qualcomm Atheros, Inc.
 * Copyright (c) 2016-2017 Erik Stromdahl <erik.stromdahl@gmail.com>
 */

/* Adapted from include/linux/compiler_attributes.h.  */
#define __aligned(x)                    __attribute__((__aligned__(x)))
#define __printf(a, b)                  __attribute__((__format__(printf, a, b)))

/* Possible macro for the new attribute.  */
#define __malloc(f)      __attribute__((malloc(f)));

/* From include/linux/types.h.  */

typedef unsigned int gfp_t;

/* Not the real value, which is in include/linux/gfp.h.  */
#define GFP_ATOMIC	32

/* From include/linux/usb.h.  */

struct urb;
extern void usb_free_urb(struct urb *urb);
extern struct urb *usb_alloc_urb(int iso_packets, gfp_t mem_flags)
  __malloc(usb_free_urb);
/* attribute added as part of testcase */

extern int usb_submit_urb(/*struct urb *urb, */gfp_t mem_flags);
extern void usb_unanchor_urb(struct urb *urb);

/* From drivers/net/wireless/ath/ath10k/core.h.  */

struct ath10k;

struct ath10k {
	/* [...many other fields removed...]  */

	/* must be last */
	u8 drv_priv[0] __aligned(sizeof(void *));
};

/* From drivers/net/wireless/ath/ath10k/debug.h.  */

enum ath10k_debug_mask {
	/* [...other values removed...]  */
	ATH10K_DBG_USB_BULK	= 0x00080000,
};

extern unsigned int ath10k_debug_mask;

__printf(3, 4) void __ath10k_dbg(struct ath10k *ar,
				 enum ath10k_debug_mask mask,
				 const char *fmt, ...);

/* Simplified for now, to avoid pulling in tracepoint code.  */
static inline
bool trace_ath10k_log_dbg_enabled(void) { return 0; }

#define ath10k_dbg(ar, dbg_mask, fmt, ...)			\
do {								\
	if ((ath10k_debug_mask & dbg_mask) ||			\
	    trace_ath10k_log_dbg_enabled())			\
		__ath10k_dbg(ar, dbg_mask, fmt, ##__VA_ARGS__); \
} while (0)

/* From drivers/net/wireless/ath/ath10k/hif.h.  */

struct ath10k_hif_sg_item {
	/* [...other fields removed...]  */
	void *transfer_context; /* NULL = tx completion callback not called */
};

struct ath10k_hif_ops {
	/* send a scatter-gather list to the target */
	int (*tx_sg)(struct ath10k *ar, u8 pipe_id,
		     struct ath10k_hif_sg_item *items, int n_items);
	/* [...other fields removed...]  */
};

/* From drivers/net/wireless/ath/ath10k/usb.h.  */

/* tx/rx pipes for usb */
enum ath10k_usb_pipe_id {
	/* [...other values removed...]  */
	ATH10K_USB_PIPE_MAX = 8
};

struct ath10k_usb_pipe {
	/* [...all fields removed...]  */
};

/* usb device object */
struct ath10k_usb {
	/* [...other fields removed...]  */
	struct ath10k_usb_pipe pipes[ATH10K_USB_PIPE_MAX];
};

/* usb urb object */
struct ath10k_urb_context {
	/* [...other fields removed...]  */
	struct ath10k_usb_pipe *pipe;
	struct sk_buff *skb;
};

static inline struct ath10k_usb *ath10k_usb_priv(struct ath10k *ar)
{
	return (struct ath10k_usb *)ar->drv_priv;
}

/* The source file.  */

static void ath10k_usb_post_recv_transfers(struct ath10k *ar,
					   struct ath10k_usb_pipe *recv_pipe);

struct ath10k_urb_context *
ath10k_usb_alloc_urb_from_pipe(struct ath10k_usb_pipe *pipe);

void ath10k_usb_free_urb_to_pipe(struct ath10k_usb_pipe *pipe,
				 struct ath10k_urb_context *urb_context);

static int ath10k_usb_hif_tx_sg(struct ath10k *ar, u8 pipe_id,
				struct ath10k_hif_sg_item *items, int n_items)
{
	struct ath10k_usb *ar_usb = ath10k_usb_priv(ar);
	struct ath10k_usb_pipe *pipe = &ar_usb->pipes[pipe_id];
	struct ath10k_urb_context *urb_context;
	struct sk_buff *skb;
	struct urb *urb;
	int ret, i;

	for (i = 0; i < n_items; i++) {
		urb_context = ath10k_usb_alloc_urb_from_pipe(pipe);
		if (!urb_context) {
			ret = -ENOMEM;
			goto err;
		}

		skb = (struct sk_buff *) items[i].transfer_context;
		urb_context->skb = skb;

		urb = usb_alloc_urb(0, GFP_ATOMIC); /* { dg-message "allocated here" } */
		if (!urb) {
			ret = -ENOMEM;
			goto err_free_urb_to_pipe;
		}

		/* TODO: these are disabled, otherwise we conservatively
		   assume that they could free urb.  */
#if 0
		usb_fill_bulk_urb(urb,
				  ar_usb->udev,
				  pipe->usb_pipe_handle,
				  skb->data,
				  skb->len,
				  ath10k_usb_transmit_complete, urb_context);
		if (!(skb->len % pipe->max_packet_size)) {
			/* hit a max packet boundary on this pipe */
			urb->transfer_flags |= URB_ZERO_PACKET;
		}

		usb_anchor_urb(urb, &pipe->urb_submitted);
#endif
		/* TODO: initial argument disabled, otherwise we conservatively
		   assume that it could free urb.  */
		ret = usb_submit_urb(/*urb, */GFP_ATOMIC);
		if (ret) { /* TODO: why doesn't it show this condition at default verbosity?  */
			ath10k_dbg(ar, ATH10K_DBG_USB_BULK,
				   "usb bulk transmit failed: %d\n", ret);

			/* TODO: this is disabled, otherwise we conservatively
			   assume that it could free urb.  */
#if 0
			usb_unanchor_urb(urb);
#endif

			ret = -EINVAL;
			/* Leak of urb happens here.  */
			goto err_free_urb_to_pipe;
		}

		usb_free_urb(urb); /* { dg-bogus "double-'usb_free_urb' of 'urb'" } */
	}

	return 0;

err_free_urb_to_pipe:
	ath10k_usb_free_urb_to_pipe(urb_context->pipe, urb_context);
err:
	return ret; /* { dg-warning "leak of 'urb'" } */
}

static const struct ath10k_hif_ops ath10k_usb_hif_ops = {
	.tx_sg			= ath10k_usb_hif_tx_sg,
};

/* Simulate code to register the callback.  */
extern void callback_registration (const void *);
void ath10k_usb_probe(void)
{
  callback_registration(&ath10k_usb_hif_ops);
}


/* The original source file ends with:
MODULE_AUTHOR("Atheros Communications, Inc.");
MODULE_DESCRIPTION("Driver support for Qualcomm Atheros 802.11ac WLAN USB devices");
MODULE_LICENSE("Dual BSD/GPL");
*/
