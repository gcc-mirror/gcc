/* This used to ICE with the ccmp patches on aarch64. */
int f(char);
int init_emit_regs (int mode) {
  f(mode == 4 || mode == 13);
}
