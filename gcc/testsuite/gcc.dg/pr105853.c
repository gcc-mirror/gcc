/* { dg-do compile } */
/* { dg-options "-fpermissive -O2" } */

struct {
  struct {
    short e16[3];
  }
} const eth_addr_zero = {{}};  /* { dg-warning "no semicolon at" } */
void compose_nd_na_ipv6_src() {
  packet_set_nd(eth_addr_zero); /* { dg-warning "implicit declaration" } */
}
