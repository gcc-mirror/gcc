// { dg-lto-do link }
/* { dg-lto-options { { -O2 -w } { -w } } } */
// { dg-extra-ld-options "-r -nostdlib" }
extern "C" char *strcpy(char *, const char *);
char InitXPCOMGlue_lastSlash;
void InitXPCOMGlue() { strcpy(&InitXPCOMGlue_lastSlash, ".so"); }
extern "C" void memcpy(void *);
char LZ4_decompress_safe_usingDict_ip;
void LZ4_decompress_safe_usingDict() {
  memcpy(&LZ4_decompress_safe_usingDict_ip);
}
