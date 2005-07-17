typedef struct dw_cfi_oprnd_struct {
      unsigned long reg;
} dw_cfa_location;
void def_cfa_1 (void) {
      dw_cfa_location loc;
        loc.reg = loc.reg;
}
