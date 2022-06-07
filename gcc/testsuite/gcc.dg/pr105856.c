/* { dg-do compile } */
/* { dg-options "-O2" } */
#pragma pack(1)
struct {
  unsigned f0;
} static g_251 = {6};
void g_329_3() {
  func_19(g_251);  /* { dg-warning "implicit declaration" } */
}

