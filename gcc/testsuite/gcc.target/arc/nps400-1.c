/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -mq-class -mbitops -munaligned-access -mcmem -O2 -fno-strict-aliasing" } */

enum npsdp_mem_space_type {
  NPSDP_EXTERNAL_MS = 1
};
struct npsdp_ext_addr {
  struct {
    struct {
      enum npsdp_mem_space_type mem_type : 1;
      unsigned msid : 5;
    };
  };
  char user_space[];
} a;
char b;
void fn1() {
  ((struct npsdp_ext_addr *)a.user_space)->mem_type = NPSDP_EXTERNAL_MS;
  ((struct npsdp_ext_addr *)a.user_space)->msid =
      ((struct npsdp_ext_addr *)a.user_space)->mem_type ? 1 : 10;
  while (b)
    ;
}
