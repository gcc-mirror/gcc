/* { dg-do compile } */
/* { dg-options "-fno-tree-sra" } */
typedef struct {
  unsigned int en : 1;
  unsigned int bit_order : 1;
  unsigned int scl_io : 1;
  unsigned int scl_inv : 1;
  unsigned int sda0_io : 1;
  unsigned int sda0_idle : 1;
  unsigned int sda1_io : 1;
  unsigned int sda1_idle : 1;
  unsigned int sda2_io : 1;
  unsigned int sda2_idle : 1;
  unsigned int sda3_io : 1;
  unsigned int sda3_idle : 1;
  unsigned int sda_sel : 2;
  unsigned int sen_idle : 1;
  unsigned int sen_inv : 1;
  unsigned int sen_sel : 2;
  unsigned int dummy1 : 14;
} reg_gio_rw_i2c1_cfg;

typedef struct {
  unsigned int data0 : 8;
  unsigned int data1 : 8;
  unsigned int data2 : 8;
  unsigned int data3 : 8;
} reg_gio_rw_i2c1_data;

typedef struct {
  unsigned int trf_bits : 6;
  unsigned int switch_dir : 6;
  unsigned int extra_start : 3;
  unsigned int early_end : 1;
  unsigned int start_stop : 1;
  unsigned int ack_dir0 : 1;
  unsigned int ack_dir1 : 1;
  unsigned int ack_dir2 : 1;
  unsigned int ack_dir3 : 1;
  unsigned int ack_dir4 : 1;
  unsigned int ack_dir5 : 1;
  unsigned int ack_bit : 1;
  unsigned int start_bit : 1;
  unsigned int freq : 2;
  unsigned int dummy1 : 5;
} reg_gio_rw_i2c1_ctrl;

extern reg_gio_rw_i2c1_cfg reg_gio;
extern reg_gio_rw_i2c1_data reg_data;
extern int reg_start;
extern reg_gio_rw_i2c1_ctrl reg_ctrl;

extern void foobar(void);
extern void foo(int);
extern void frob(unsigned int);
extern void bar(int);
extern void baz(void);

unsigned int f(int *devspec, unsigned int addr)
{
 reg_gio_rw_i2c1_ctrl ctrl = {0};
 reg_gio_rw_i2c1_data data = {0};

 foobar();

 static int first = 1;

 if (first) {
  reg_gio_rw_i2c1_cfg cfg = {0};
  first = 0;

  foo(1);
  cfg.sda0_idle = 1;
  cfg.sda0_io = 0;
  cfg.scl_inv = 0;
  cfg.scl_io = 0;
  cfg.bit_order = 1;
  cfg.sda_sel = 0;
  cfg.sen_sel = 0;
  cfg.en = 1;
  reg_gio = cfg;
 }

 ctrl.freq = 1;
 ctrl.start_bit = 0;
 ctrl.ack_bit = 1;
 ctrl.ack_dir0 = 0;
 ctrl.ack_dir1 = 0;
 ctrl.ack_dir2 = 0;
 ctrl.ack_dir3 = 1;
 ctrl.ack_dir4 = 0;
 ctrl.ack_dir5 = 0;
 ctrl.start_stop = 1;
 ctrl.early_end = 0;
 ctrl.extra_start = 2;
 ctrl.switch_dir = 8*3;
 ctrl.trf_bits = 8*4;
 reg_ctrl = ctrl;
 frob(0xac);
 data.data0 = devspec[1] & 192;
 data.data1 = addr;
 data.data2 = devspec[1] | 0x01;
 reg_data = data;
 reg_start = 1;
 bar(100);
 data = reg_data;
 baz();

 return data.data3;
}
