struct T {
  unsigned char f[1] __attribute__((packed)); /* { dg-warning "ignored for field of type.*unsigned char\\\[1\\\]" } */
  unsigned char g[14] __attribute__((packed)); /* { dg-warning "ignored for field of type.*unsigned char\\\[14\\\]" } */
};
