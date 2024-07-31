typedef unsigned u32;
int list_is_head();
void tu102_acr_wpr_build_acr_0_0_0(int, long, u32);
void tu102_acr_wpr_build() {
  u32 offset = 0;
  for (; list_is_head();) {
    int hdr;
    u32 _addr = offset, _size = sizeof(hdr), *_data = &hdr;
    while (_size--) {
      tu102_acr_wpr_build_acr_0_0_0(0, _addr, *_data++);
      _addr += 4;
    }
    offset += sizeof(hdr);
  }
  tu102_acr_wpr_build_acr_0_0_0(0, offset, 0);
}
