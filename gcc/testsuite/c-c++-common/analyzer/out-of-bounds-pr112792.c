/* PR analyzer/112792 ("-Wanalyzer-out-of-bounds false positives seen on
   Linux kernel with certain unions").  */

typedef unsigned int u32;

union msix_perm {
  struct {
    u32 rsvd2 : 8;
    u32 pasid : 20;
  };
  u32 bits;
} __attribute__((__packed__));

union msix_perm mperm;

void idxd_device_set_perm_entry(u32 pasid) {
  mperm.pasid = pasid; /* { dg-bogus "buffer-overflow" } */
}
