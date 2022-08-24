/* Reproducer for report from -Wanalyzer-malloc-leak
   Reduced from
     https://git.qemu.org/?p=qemu.git;a=blob;f=subprojects/libvhost-user/libvhost-user.c;h=fab7ca17ee1fb27bcfc338527d1aeb9f923aade5;hb=HEAD#l1184
   which is licensed under GNU GPLv2 or later. */

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned long uint64_t;
typedef unsigned long uint64_t;
typedef __SIZE_TYPE__ size_t;

extern void *calloc(size_t __nmemb, size_t __size)
  __attribute__((__nothrow__, __leaf__))
  __attribute__((__malloc__))
  __attribute__((__alloc_size__(1, 2)))
  __attribute__((__warn_unused_result__));

typedef struct VuDescStateSplit {
  uint8_t inflight;
  uint64_t counter;
} VuDescStateSplit;

typedef struct VuVirtqInflight {
  uint16_t desc_num;
  VuDescStateSplit desc[];
} VuVirtqInflight;

typedef struct VuVirtqInflightDesc {
  uint16_t index;
  uint64_t counter;
} VuVirtqInflightDesc;

typedef struct VuVirtq {
  VuVirtqInflight *inflight;
  VuVirtqInflightDesc *resubmit_list;
  uint16_t resubmit_num;
  uint64_t counter;
  int inuse;
} VuVirtq;

int vu_check_queue_inflights(VuVirtq *vq) {
  int i = 0;

  if (vq->inuse) {
    vq->resubmit_list = calloc(vq->inuse, sizeof(VuVirtqInflightDesc));
    if (!vq->resubmit_list) {
      return -1;
    }

    for (i = 0; i < vq->inflight->desc_num; i++) {
      if (vq->inflight->desc[i].inflight) {
        vq->resubmit_list[vq->resubmit_num].index = i; /* { dg-bogus "leak" } */
        vq->resubmit_list[vq->resubmit_num].counter =
            vq->inflight->desc[i].counter;
        vq->resubmit_num++;
      }
    }
  }

  return 0;
}
