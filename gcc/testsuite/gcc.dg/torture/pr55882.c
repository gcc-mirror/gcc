/* { dg-do run } */

typedef enum
{
  PVT_A = 0,
  PVT_B = 1,
  PVT_CONFIG = 2,
  PVT_RESERVED3 = 3,
} T_CR_SELECT;

typedef enum
{
  STD_ULOGIC_0 = 0,
  STD_ULOGIC_1 = 1,
} STD_ULOGIC;

typedef struct
{
  unsigned char rtp : 3;
  unsigned char rtn : 3;
} C;

typedef struct
{
  unsigned char nd;
  unsigned char pd;
  unsigned char rtn;
  unsigned char rtp;
} A;

typedef struct
{
  unsigned short reserved : 14;
  unsigned char Z_rx_enable : 2;
  A pvt;
} B;

typedef struct
{
  B cr_dsclk_q3;
  B cr_data_q3;
  B cr_addr_q3;
  B cr_cmd_q3;
  B cr_pres_q3;
  C cr_vref_q3[6];
  unsigned char pres_disable;
  unsigned char pres_drive_high;
  unsigned char c_enab_120;
  STD_ULOGIC clk_tximp;
  STD_ULOGIC dqs_tximp;
  STD_ULOGIC cmd_tximp;
  STD_ULOGIC data_tximp;
  STD_ULOGIC dqs_rxterm;
  STD_ULOGIC data_rxterm;
  T_CR_SELECT cr_clk_sel;
  unsigned char cr_clk : 5;
  T_CR_SELECT cr_dsclk_odd_sel;
  unsigned char cr_dsclk_odd : 5;
  T_CR_SELECT cr_dsclk_even_sel;
  unsigned char cr_dsclk_even : 5;
  T_CR_SELECT cr_data_sel;
  unsigned char cr_data : 5;
  T_CR_SELECT cr_vref_sel;
  unsigned char cr_vref : 5;
  T_CR_SELECT cr_others_sel;
  unsigned char cr_others : 5;
} CONFIG;

typedef struct
{
  unsigned char enable_monitor;
  unsigned short step_out_pointer : 12;
  unsigned short hold_out_pointer : 12;
  unsigned short enable_wr_dqs : 12;
  unsigned short use_alt_rd_dqs : 12;
  CONFIG io_buf;
} mystruct;

unsigned short __attribute__((noinline,noclone))
testfunction(unsigned i)
{
  mystruct dmfe[8];
  dmfe[0].use_alt_rd_dqs = 1;
  dmfe[i].use_alt_rd_dqs = 0;
  return dmfe[0].use_alt_rd_dqs;
}

extern void abort (void);
int main ()
{
  if (testfunction(0) != 0) 
    abort ();
  return 0;
}
