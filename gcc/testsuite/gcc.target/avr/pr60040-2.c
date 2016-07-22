/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned char __uint8_t;
typedef short unsigned int __uint16_t;
typedef long unsigned int __uint32_t;
typedef __uint8_t uint8_t ;
typedef __uint16_t uint16_t ;
typedef __uint32_t uint32_t ;
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
typedef enum rtems_blkdev_request_op {
  RTEMS_BLKDEV_REQ_READ,
} rtems_fdisk_segment_desc;
typedef struct rtems_fdisk_driver_handlers
{
  int (*blank) (const rtems_fdisk_segment_desc* sd,
                uint32_t device,
                uint32_t segment,
                uint32_t offset,
                uint32_t size);
} rtems_fdisk_driver_handlers;
typedef struct rtems_fdisk_page_desc
{
  uint16_t flags;
  uint32_t block;
} rtems_fdisk_page_desc;
typedef struct rtems_fdisk_segment_ctl
{
  rtems_fdisk_page_desc* page_descriptors;
  uint32_t pages_active;
} rtems_fdisk_segment_ctl;
typedef struct rtems_fdisk_segment_ctl_queue
{
} rtems_fdisk_segment_ctl_queue;
typedef struct rtems_fdisk_device_ctl
{
  uint32_t flags;
  uint8_t* copy_buffer;
} rtems_flashdisk;

extern void rtems_fdisk_error (const char *, ...);
extern int rtems_fdisk_seg_write(const rtems_flashdisk*,
                                 rtems_fdisk_segment_ctl*,
                                 uint32_t,
                                 const rtems_fdisk_page_desc* page_desc,
				 uint32_t);

void rtems_fdisk_printf (const rtems_flashdisk* fd, const char *format, ...)
{
  {
    va_list args;
    __builtin_va_start(args,format);
  }
}
static int
rtems_fdisk_seg_blank_check (const rtems_flashdisk* fd,
                             rtems_fdisk_segment_ctl* sc,
                             uint32_t offset,
                             uint32_t size)
{
  uint32_t device;
  uint32_t segment;
  const rtems_fdisk_segment_desc* sd;
  const rtems_fdisk_driver_handlers* ops;
  return ops->blank (sd, device, segment, offset, size);
}
static int
rtems_fdisk_seg_write_page_desc (const rtems_flashdisk* fd,
                                 rtems_fdisk_segment_ctl* sc,
                                 uint32_t page,
                                 const rtems_fdisk_page_desc* page_desc)
{
  uint32_t offset = page * sizeof (rtems_fdisk_page_desc);
  if ((fd->flags & (1 << 3)))
  {
    int ret = rtems_fdisk_seg_blank_check (fd, sc,
                                           offset,
                                           sizeof (rtems_fdisk_page_desc));
  }
  return rtems_fdisk_seg_write (fd, sc, offset,
                                page_desc, sizeof (rtems_fdisk_page_desc));
}
void
rtems_fdisk_recycle_segment (rtems_flashdisk* fd,
                                    rtems_fdisk_segment_ctl* ssc,
                                    rtems_fdisk_segment_ctl* dsc,
                                    uint32_t *pages)
{
  int ret;
  uint32_t spage;
  uint32_t used = 0;
  uint32_t active = 0;
  {
    rtems_fdisk_page_desc* spd = &ssc->page_descriptors[spage];
    {
      rtems_fdisk_page_desc* dpd;
      uint32_t dpage;
      dpd = &dsc->page_descriptors[dpage];
      *dpd = *spd;
      ret = rtems_fdisk_seg_write_page_desc (fd,
                                             dsc,
                                             dpage, dpd);
    }
  }
  rtems_fdisk_printf (fd, "ssc end: %d-%d: p=%ld, a=%ld, u=%ld",
                      pages, active, used);
  {
    rtems_fdisk_error ("compacting: ssc pages not 0: %d",
                       ssc->pages_active);
  }
}
