/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -O3 -funroll-loops -ftracer" } */

struct platform_device;
typedef unsigned long __kernel_size_t;
typedef unsigned short __u16;
typedef unsigned int __u32;
typedef unsigned char u8;
typedef unsigned short u16;
typedef __kernel_size_t size_t;
typedef __u32 uint32_t;
static inline __attribute__ ((always_inline))
uint32_t __attribute__ ((pure)) bfin_dspid (void)
{
    return ( {
            uint32_t __v; __v;}
    );
}
struct list_head {
    struct list_head *next, *prev;
};
struct page {
    union {
    };
    struct list_head lru;
};
struct device_driver {
    const char *name;
    struct module *owner;
};
struct fb_info {
    struct device *dev;
};
struct platform_driver {
    int (*probe) (struct platform_device *);
    int (*remove) (struct platform_device *);
    struct device_driver driver;
};
struct firmware {
    size_t size;
    const u8 *data;
};
struct metronomefb_par {
    struct fb_info *info;
};
struct waveform_hdr {
    u8 trc;
};
static u8 calc_cksum (int start, int end, u8 * mem)
{
    u8 tmp = 0;
    int i;
    for (i = start; i < end; i++)
        tmp += mem[i];
    return tmp;
}
extern struct waveform_hdr *wfm_hdr;
extern int wmta;

static int
load_waveform (u8 * mem, size_t size, int m, int t, struct metronomefb_par *par)
{
    int tta;
    int trn = 0;
    int i;
    u8 cksum;
    int cksum_idx;
    struct device *dev = par->info->dev;
    for (i = 0; i <= sizeof (*wfm_hdr) + wfm_hdr->trc; i++) {
        if (mem[i] > t) {
            trn = i - sizeof (*wfm_hdr) - 1;
        }
    }
    tta = * (mem + wmta + m * 4) & 0x00FFFFFF;
    cksum_idx = tta + trn * 4 + 3;
    cksum = calc_cksum (cksum_idx - 3, cksum_idx, mem);
    if (cksum != mem[cksum_idx]) {
        __builtin_abort();
    }
}
extern struct firmware *fw_entry;
extern struct metronomefb_par *par;

int metronomefb_probe (struct platform_device *dev)
{
        return load_waveform ((u8 *) fw_entry->data, fw_entry->size, 3, 31, par);
}
