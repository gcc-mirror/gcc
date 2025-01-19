/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - nvme_mi
#include <stdlib.h>
typedef unsigned int __u32;
typedef unsigned char __u8;
struct nvme_mi_msg_hdr {
    __u8 type;
    __u8 nmp;
    __u8 meb;
    __u8 rsvd0;
} __attribute__((packed));

struct nvme_mi_req {
    struct nvme_mi_msg_hdr *hdr;
    size_t hdr_len;
    void *data;
    size_t data_len;
    __u32 mic;
};
__u32 nvme_mi_crc32_update (__u32 crc, void *data, size_t len)
{
  int i;

  while (len--)
    {
      crc ^= *(unsigned char *) (data++);
      for (i = 0; i < 8; i++)
	crc = (crc >> 1) ^ ((crc & 1) ? 0x82F63B78 : 0);
    }
  return crc;
}

void nvme_mi_calc_req_mic (struct nvme_mi_req *req)
{
  if (sizeof (__u32) < 4)
    exit (0);

  __u32 crc = 0xffffffff;

  crc = nvme_mi_crc32_update (crc, req->hdr, req->hdr_len);
  crc = nvme_mi_crc32_update (crc, req->data, req->data_len);

  req->mic = ~crc;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
