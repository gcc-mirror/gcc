/* Subroutines used for code generation on TI MSP430 processors.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "gimple-expr.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "regs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "output.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "builtins.h"
#include "intl.h"

/* This file should be included last.  */
#include "target-def.h"


static void msp430_compute_frame_info (void);



/* Run-time Target Specification.  */

bool msp430x = true;

struct GTY(()) machine_function
{
  /* If set, the rest of the fields have been computed.  */
  int computed;
  /* Which registers need to be saved in the pro/epilogue.  */
  int need_to_save [FIRST_PSEUDO_REGISTER];

  /* These fields describe the frame layout...  */
  /* arg pointer */
  /* 2/4 bytes for saved PC */
  int framesize_regs;
  /* frame pointer */
  int framesize_locals;
  int framesize_outgoing;
  /* stack pointer */
  int framesize;

  /* How much we adjust the stack when returning from an exception
     handler.  */
  rtx eh_stack_adjust;
};

/* This is our init_machine_status, as set in
   msp_option_override.  */
static struct machine_function *
msp430_init_machine_status (void)
{
  struct machine_function *m;

  m = ggc_cleared_alloc<machine_function> ();

  return m;
}

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE		msp430_option_override

/* This is a copy of the same data structure found in gas/config/tc-msp430.c
   Also another (sort-of) copy can be found in gcc/config/msp430/t-msp430
   Keep these three structures in sync.
   The data in this structure has been extracted from version 1.194 of the
   devices.csv file released by TI in September 2016.  */

struct msp430_mcu_data
{
  const char * name;
  unsigned int revision; /* 0=> MSP430, 1=>MSP430X, 2=> MSP430Xv2.  */
  unsigned int hwmpy;    /* 0=>none, 1=>16-bit, 2=>16-bit w/sign extend, 4=>32-bit, 8=> 32-bit (5xx).  */
}
msp430_mcu_data [] =
{
  { "cc430f5123",2,8 },
  { "cc430f5125",2,8 },
  { "cc430f5133",2,8 },
  { "cc430f5135",2,8 },
  { "cc430f5137",2,8 },
  { "cc430f5143",2,8 },
  { "cc430f5145",2,8 },
  { "cc430f5147",2,8 },
  { "cc430f6125",2,8 },
  { "cc430f6126",2,8 },
  { "cc430f6127",2,8 },
  { "cc430f6135",2,8 },
  { "cc430f6137",2,8 },
  { "cc430f6143",2,8 },
  { "cc430f6145",2,8 },
  { "cc430f6147",2,8 },
  { "msp430afe221",0,2 },
  { "msp430afe222",0,2 },
  { "msp430afe223",0,2 },
  { "msp430afe231",0,2 },
  { "msp430afe232",0,2 },
  { "msp430afe233",0,2 },
  { "msp430afe251",0,2 },
  { "msp430afe252",0,2 },
  { "msp430afe253",0,2 },
  { "msp430bt5190",2,8 },
  { "msp430c091",0,0 },
  { "msp430c092",0,0 },
  { "msp430c111",0,0 },
  { "msp430c1111",0,0 },
  { "msp430c112",0,0 },
  { "msp430c1121",0,0 },
  { "msp430c1331",0,0 },
  { "msp430c1351",0,0 },
  { "msp430c311s",0,0 },
  { "msp430c312",0,0 },
  { "msp430c313",0,0 },
  { "msp430c314",0,0 },
  { "msp430c315",0,0 },
  { "msp430c323",0,0 },
  { "msp430c325",0,0 },
  { "msp430c336",0,1 },
  { "msp430c337",0,1 },
  { "msp430c412",0,0 },
  { "msp430c413",0,0 },
  { "msp430cg4616",1,1 },
  { "msp430cg4617",1,1 },
  { "msp430cg4618",1,1 },
  { "msp430cg4619",1,1 },
  { "msp430e112",0,0 },
  { "msp430e313",0,0 },
  { "msp430e315",0,0 },
  { "msp430e325",0,0 },
  { "msp430e337",0,1 },
  { "msp430f110",0,0 },
  { "msp430f1101",0,0 },
  { "msp430f1101a",0,0 },
  { "msp430f1111",0,0 },
  { "msp430f1111a",0,0 },
  { "msp430f112",0,0 },
  { "msp430f1121",0,0 },
  { "msp430f1121a",0,0 },
  { "msp430f1122",0,0 },
  { "msp430f1132",0,0 },
  { "msp430f122",0,0 },
  { "msp430f1222",0,0 },
  { "msp430f123",0,0 },
  { "msp430f1232",0,0 },
  { "msp430f133",0,0 },
  { "msp430f135",0,0 },
  { "msp430f147",0,1 },
  { "msp430f1471",0,1 },
  { "msp430f148",0,1 },
  { "msp430f1481",0,1 },
  { "msp430f149",0,1 },
  { "msp430f1491",0,1 },
  { "msp430f155",0,0 },
  { "msp430f156",0,0 },
  { "msp430f157",0,0 },
  { "msp430f1610",0,1 },
  { "msp430f1611",0,1 },
  { "msp430f1612",0,1 },
  { "msp430f167",0,1 },
  { "msp430f168",0,1 },
  { "msp430f169",0,1 },
  { "msp430f2001",0,0 },
  { "msp430f2002",0,0 },
  { "msp430f2003",0,0 },
  { "msp430f2011",0,0 },
  { "msp430f2012",0,0 },
  { "msp430f2013",0,0 },
  { "msp430f2101",0,0 },
  { "msp430f2111",0,0 },
  { "msp430f2112",0,0 },
  { "msp430f2121",0,0 },
  { "msp430f2122",0,0 },
  { "msp430f2131",0,0 },
  { "msp430f2132",0,0 },
  { "msp430f2232",0,0 },
  { "msp430f2234",0,0 },
  { "msp430f2252",0,0 },
  { "msp430f2254",0,0 },
  { "msp430f2272",0,0 },
  { "msp430f2274",0,0 },
  { "msp430f233",0,2 },
  { "msp430f2330",0,2 },
  { "msp430f235",0,2 },
  { "msp430f2350",0,2 },
  { "msp430f2370",0,2 },
  { "msp430f2410",0,2 },
  { "msp430f2416",1,2 },
  { "msp430f2417",1,2 },
  { "msp430f2418",1,2 },
  { "msp430f2419",1,2 },
  { "msp430f247",0,2 },
  { "msp430f2471",0,2 },
  { "msp430f248",0,2 },
  { "msp430f2481",0,2 },
  { "msp430f249",0,2 },
  { "msp430f2491",0,2 },
  { "msp430f2616",1,2 },
  { "msp430f2617",1,2 },
  { "msp430f2618",1,2 },
  { "msp430f2619",1,2 },
  { "msp430f412",0,0 },
  { "msp430f413",0,0 },
  { "msp430f4132",0,0 },
  { "msp430f415",0,0 },
  { "msp430f4152",0,0 },
  { "msp430f417",0,0 },
  { "msp430f423",0,1 },
  { "msp430f423a",0,1 },
  { "msp430f425",0,1 },
  { "msp430f4250",0,0 },
  { "msp430f425a",0,1 },
  { "msp430f4260",0,0 },
  { "msp430f427",0,1 },
  { "msp430f4270",0,0 },
  { "msp430f427a",0,1 },
  { "msp430f435",0,0 },
  { "msp430f4351",0,0 },
  { "msp430f436",0,0 },
  { "msp430f4361",0,0 },
  { "msp430f437",0,0 },
  { "msp430f4371",0,0 },
  { "msp430f438",0,0 },
  { "msp430f439",0,0 },
  { "msp430f447",0,1 },
  { "msp430f448",0,1 },
  { "msp430f4481",0,1 },
  { "msp430f449",0,1 },
  { "msp430f4491",0,1 },
  { "msp430f4616",1,1 },
  { "msp430f46161",1,1 },
  { "msp430f4617",1,1 },
  { "msp430f46171",1,1 },
  { "msp430f4618",1,1 },
  { "msp430f46181",1,1 },
  { "msp430f4619",1,1 },
  { "msp430f46191",1,1 },
  { "msp430f47126",1,4 },
  { "msp430f47127",1,4 },
  { "msp430f47163",1,4 },
  { "msp430f47166",1,4 },
  { "msp430f47167",1,4 },
  { "msp430f47173",1,4 },
  { "msp430f47176",1,4 },
  { "msp430f47177",1,4 },
  { "msp430f47183",1,4 },
  { "msp430f47186",1,4 },
  { "msp430f47187",1,4 },
  { "msp430f47193",1,4 },
  { "msp430f47196",1,4 },
  { "msp430f47197",1,4 },
  { "msp430f477",0,0 },
  { "msp430f478",0,0 },
  { "msp430f4783",0,4 },
  { "msp430f4784",0,4 },
  { "msp430f479",0,0 },
  { "msp430f4793",0,4 },
  { "msp430f4794",0,4 },
  { "msp430f5131",2,8 },
  { "msp430f5132",2,8 },
  { "msp430f5151",2,8 },
  { "msp430f5152",2,8 },
  { "msp430f5171",2,8 },
  { "msp430f5172",2,8 },
  { "msp430f5212",2,8 },
  { "msp430f5213",2,8 },
  { "msp430f5214",2,8 },
  { "msp430f5217",2,8 },
  { "msp430f5218",2,8 },
  { "msp430f5219",2,8 },
  { "msp430f5222",2,8 },
  { "msp430f5223",2,8 },
  { "msp430f5224",2,8 },
  { "msp430f5227",2,8 },
  { "msp430f5228",2,8 },
  { "msp430f5229",2,8 },
  { "msp430f5232",2,8 },
  { "msp430f5234",2,8 },
  { "msp430f5237",2,8 },
  { "msp430f5239",2,8 },
  { "msp430f5242",2,8 },
  { "msp430f5244",2,8 },
  { "msp430f5247",2,8 },
  { "msp430f5249",2,8 },
  { "msp430f5252",2,8 },
  { "msp430f5253",2,8 },
  { "msp430f5254",2,8 },
  { "msp430f5255",2,8 },
  { "msp430f5256",2,8 },
  { "msp430f5257",2,8 },
  { "msp430f5258",2,8 },
  { "msp430f5259",2,8 },
  { "msp430f5304",2,8 },
  { "msp430f5308",2,8 },
  { "msp430f5309",2,8 },
  { "msp430f5310",2,8 },
  { "msp430f5324",2,8 },
  { "msp430f5325",2,8 },
  { "msp430f5326",2,8 },
  { "msp430f5327",2,8 },
  { "msp430f5328",2,8 },
  { "msp430f5329",2,8 },
  { "msp430f5333",2,8 },
  { "msp430f5335",2,8 },
  { "msp430f5336",2,8 },
  { "msp430f5338",2,8 },
  { "msp430f5340",2,8 },
  { "msp430f5341",2,8 },
  { "msp430f5342",2,8 },
  { "msp430f5358",2,8 },
  { "msp430f5359",2,8 },
  { "msp430f5418",2,8 },
  { "msp430f5418a",2,8 },
  { "msp430f5419",2,8 },
  { "msp430f5419a",2,8 },
  { "msp430f5435",2,8 },
  { "msp430f5435a",2,8 },
  { "msp430f5436",2,8 },
  { "msp430f5436a",2,8 },
  { "msp430f5437",2,8 },
  { "msp430f5437a",2,8 },
  { "msp430f5438",2,8 },
  { "msp430f5438a",2,8 },
  { "msp430f5500",2,8 },
  { "msp430f5501",2,8 },
  { "msp430f5502",2,8 },
  { "msp430f5503",2,8 },
  { "msp430f5504",2,8 },
  { "msp430f5505",2,8 },
  { "msp430f5506",2,8 },
  { "msp430f5507",2,8 },
  { "msp430f5508",2,8 },
  { "msp430f5509",2,8 },
  { "msp430f5510",2,8 },
  { "msp430f5513",2,8 },
  { "msp430f5514",2,8 },
  { "msp430f5515",2,8 },
  { "msp430f5517",2,8 },
  { "msp430f5519",2,8 },
  { "msp430f5521",2,8 },
  { "msp430f5522",2,8 },
  { "msp430f5524",2,8 },
  { "msp430f5525",2,8 },
  { "msp430f5526",2,8 },
  { "msp430f5527",2,8 },
  { "msp430f5528",2,8 },
  { "msp430f5529",2,8 },
  { "msp430f5630",2,8 },
  { "msp430f5631",2,8 },
  { "msp430f5632",2,8 },
  { "msp430f5633",2,8 },
  { "msp430f5634",2,8 },
  { "msp430f5635",2,8 },
  { "msp430f5636",2,8 },
  { "msp430f5637",2,8 },
  { "msp430f5638",2,8 },
  { "msp430f5658",2,8 },
  { "msp430f5659",2,8 },
  { "msp430f5xx_6xxgeneric",2,8 },
  { "msp430f6433",2,8 },
  { "msp430f6435",2,8 },
  { "msp430f6436",2,8 },
  { "msp430f6438",2,8 },
  { "msp430f6458",2,8 },
  { "msp430f6459",2,8 },
  { "msp430f6630",2,8 },
  { "msp430f6631",2,8 },
  { "msp430f6632",2,8 },
  { "msp430f6633",2,8 },
  { "msp430f6634",2,8 },
  { "msp430f6635",2,8 },
  { "msp430f6636",2,8 },
  { "msp430f6637",2,8 },
  { "msp430f6638",2,8 },
  { "msp430f6658",2,8 },
  { "msp430f6659",2,8 },
  { "msp430f6720",2,8 },
  { "msp430f6720a",2,8 },
  { "msp430f6721",2,8 },
  { "msp430f6721a",2,8 },
  { "msp430f6723",2,8 },
  { "msp430f6723a",2,8 },
  { "msp430f6724",2,8 },
  { "msp430f6724a",2,8 },
  { "msp430f6725",2,8 },
  { "msp430f6725a",2,8 },
  { "msp430f6726",2,8 },
  { "msp430f6726a",2,8 },
  { "msp430f6730",2,8 },
  { "msp430f6730a",2,8 },
  { "msp430f6731",2,8 },
  { "msp430f6731a",2,8 },
  { "msp430f6733",2,8 },
  { "msp430f6733a",2,8 },
  { "msp430f6734",2,8 },
  { "msp430f6734a",2,8 },
  { "msp430f6735",2,8 },
  { "msp430f6735a",2,8 },
  { "msp430f6736",2,8 },
  { "msp430f6736a",2,8 },
  { "msp430f6745",2,8 },
  { "msp430f67451",2,8 },
  { "msp430f67451a",2,8 },
  { "msp430f6745a",2,8 },
  { "msp430f6746",2,8 },
  { "msp430f67461",2,8 },
  { "msp430f67461a",2,8 },
  { "msp430f6746a",2,8 },
  { "msp430f6747",2,8 },
  { "msp430f67471",2,8 },
  { "msp430f67471a",2,8 },
  { "msp430f6747a",2,8 },
  { "msp430f6748",2,8 },
  { "msp430f67481",2,8 },
  { "msp430f67481a",2,8 },
  { "msp430f6748a",2,8 },
  { "msp430f6749",2,8 },
  { "msp430f67491",2,8 },
  { "msp430f67491a",2,8 },
  { "msp430f6749a",2,8 },
  { "msp430f67621",2,8 },
  { "msp430f67621a",2,8 },
  { "msp430f67641",2,8 },
  { "msp430f67641a",2,8 },
  { "msp430f6765",2,8 },
  { "msp430f67651",2,8 },
  { "msp430f67651a",2,8 },
  { "msp430f6765a",2,8 },
  { "msp430f6766",2,8 },
  { "msp430f67661",2,8 },
  { "msp430f67661a",2,8 },
  { "msp430f6766a",2,8 },
  { "msp430f6767",2,8 },
  { "msp430f67671",2,8 },
  { "msp430f67671a",2,8 },
  { "msp430f6767a",2,8 },
  { "msp430f6768",2,8 },
  { "msp430f67681",2,8 },
  { "msp430f67681a",2,8 },
  { "msp430f6768a",2,8 },
  { "msp430f6769",2,8 },
  { "msp430f67691",2,8 },
  { "msp430f67691a",2,8 },
  { "msp430f6769a",2,8 },
  { "msp430f6775",2,8 },
  { "msp430f67751",2,8 },
  { "msp430f67751a",2,8 },
  { "msp430f6775a",2,8 },
  { "msp430f6776",2,8 },
  { "msp430f67761",2,8 },
  { "msp430f67761a",2,8 },
  { "msp430f6776a",2,8 },
  { "msp430f6777",2,8 },
  { "msp430f67771",2,8 },
  { "msp430f67771a",2,8 },
  { "msp430f6777a",2,8 },
  { "msp430f6778",2,8 },
  { "msp430f67781",2,8 },
  { "msp430f67781a",2,8 },
  { "msp430f6778a",2,8 },
  { "msp430f6779",2,8 },
  { "msp430f67791",2,8 },
  { "msp430f67791a",2,8 },
  { "msp430f6779a",2,8 },
  { "msp430fe423",0,0 },
  { "msp430fe4232",0,0 },
  { "msp430fe423a",0,0 },
  { "msp430fe4242",0,0 },
  { "msp430fe425",0,0 },
  { "msp430fe4252",0,0 },
  { "msp430fe425a",0,0 },
  { "msp430fe427",0,0 },
  { "msp430fe4272",0,0 },
  { "msp430fe427a",0,0 },
  { "msp430fg4250",0,0 },
  { "msp430fg4260",0,0 },
  { "msp430fg4270",0,0 },
  { "msp430fg437",0,0 },
  { "msp430fg438",0,0 },
  { "msp430fg439",0,0 },
  { "msp430fg4616",1,1 },
  { "msp430fg4617",1,1 },
  { "msp430fg4618",1,1 },
  { "msp430fg4619",1,1 },
  { "msp430fg477",0,0 },
  { "msp430fg478",0,0 },
  { "msp430fg479",0,0 },
  { "msp430fg6425",2,8 },
  { "msp430fg6426",2,8 },
  { "msp430fg6625",2,8 },
  { "msp430fg6626",2,8 },
  { "msp430fr2032",2,0 },
  { "msp430fr2033",2,0 },
  { "msp430fr2110",2,0 },
  { "msp430fr2111",2,0 },
  { "msp430fr2310",2,0 },
  { "msp430fr2311",2,0 },
  { "msp430fr2433",2,8 },
  { "msp430fr2532",2,8 },
  { "msp430fr2533",2,8 },
  { "msp430fr2632",2,8 },
  { "msp430fr2633",2,8 },
  { "msp430fr2xx_4xxgeneric",2,8 },
  { "msp430fr4131",2,0 },
  { "msp430fr4132",2,0 },
  { "msp430fr4133",2,0 },
  { "msp430fr5720",2,8 },
  { "msp430fr5721",2,8 },
  { "msp430fr5722",2,8 },
  { "msp430fr5723",2,8 },
  { "msp430fr5724",2,8 },
  { "msp430fr5725",2,8 },
  { "msp430fr5726",2,8 },
  { "msp430fr5727",2,8 },
  { "msp430fr5728",2,8 },
  { "msp430fr5729",2,8 },
  { "msp430fr5730",2,8 },
  { "msp430fr5731",2,8 },
  { "msp430fr5732",2,8 },
  { "msp430fr5733",2,8 },
  { "msp430fr5734",2,8 },
  { "msp430fr5735",2,8 },
  { "msp430fr5736",2,8 },
  { "msp430fr5737",2,8 },
  { "msp430fr5738",2,8 },
  { "msp430fr5739",2,8 },
  { "msp430fr57xxgeneric",2,8 },
  { "msp430fr5847",2,8 },
  { "msp430fr58471",2,8 },
  { "msp430fr5848",2,8 },
  { "msp430fr5849",2,8 },
  { "msp430fr5857",2,8 },
  { "msp430fr5858",2,8 },
  { "msp430fr5859",2,8 },
  { "msp430fr5867",2,8 },
  { "msp430fr58671",2,8 },
  { "msp430fr5868",2,8 },
  { "msp430fr5869",2,8 },
  { "msp430fr5870",2,8 },
  { "msp430fr5872",2,8 },
  { "msp430fr58721",2,8 },
  { "msp430fr5887",2,8 },
  { "msp430fr5888",2,8 },
  { "msp430fr5889",2,8 },
  { "msp430fr58891",2,8 },
  { "msp430fr5922",2,8 },
  { "msp430fr59221",2,8 },
  { "msp430fr5947",2,8 },
  { "msp430fr59471",2,8 },
  { "msp430fr5948",2,8 },
  { "msp430fr5949",2,8 },
  { "msp430fr5957",2,8 },
  { "msp430fr5958",2,8 },
  { "msp430fr5959",2,8 },
  { "msp430fr5962",2,8 },
  { "msp430fr5964",2,8 },
  { "msp430fr5967",2,8 },
  { "msp430fr5968",2,8 },
  { "msp430fr5969",2,8 },
  { "msp430fr59691",2,8 },
  { "msp430fr5970",2,8 },
  { "msp430fr5972",2,8 },
  { "msp430fr59721",2,8 },
  { "msp430fr5986",2,8 },
  { "msp430fr5987",2,8 },
  { "msp430fr5988",2,8 },
  { "msp430fr5989",2,8 },
  { "msp430fr59891",2,8 },
  { "msp430fr5992",2,8 },
  { "msp430fr5994",2,8 },
  { "msp430fr59941",2,8 },
  { "msp430fr5xx_6xxgeneric",2,8 },
  { "msp430fr6820",2,8 },
  { "msp430fr6822",2,8 },
  { "msp430fr68221",2,8 },
  { "msp430fr6870",2,8 },
  { "msp430fr6872",2,8 },
  { "msp430fr68721",2,8 },
  { "msp430fr6877",2,8 },
  { "msp430fr6879",2,8 },
  { "msp430fr68791",2,8 },
  { "msp430fr6887",2,8 },
  { "msp430fr6888",2,8 },
  { "msp430fr6889",2,8 },
  { "msp430fr68891",2,8 },
  { "msp430fr6920",2,8 },
  { "msp430fr6922",2,8 },
  { "msp430fr69221",2,8 },
  { "msp430fr6927",2,8 },
  { "msp430fr69271",2,8 },
  { "msp430fr6928",2,8 },
  { "msp430fr6970",2,8 },
  { "msp430fr6972",2,8 },
  { "msp430fr69721",2,8 },
  { "msp430fr6977",2,8 },
  { "msp430fr6979",2,8 },
  { "msp430fr69791",2,8 },
  { "msp430fr6987",2,8 },
  { "msp430fr6988",2,8 },
  { "msp430fr6989",2,8 },
  { "msp430fr69891",2,8 },
  { "msp430fw423",0,0 },
  { "msp430fw425",0,0 },
  { "msp430fw427",0,0 },
  { "msp430fw428",0,0 },
  { "msp430fw429",0,0 },
  { "msp430g2001",0,0 },
  { "msp430g2101",0,0 },
  { "msp430g2102",0,0 },
  { "msp430g2111",0,0 },
  { "msp430g2112",0,0 },
  { "msp430g2113",0,0 },
  { "msp430g2121",0,0 },
  { "msp430g2131",0,0 },
  { "msp430g2132",0,0 },
  { "msp430g2152",0,0 },
  { "msp430g2153",0,0 },
  { "msp430g2201",0,0 },
  { "msp430g2202",0,0 },
  { "msp430g2203",0,0 },
  { "msp430g2210",0,0 },
  { "msp430g2211",0,0 },
  { "msp430g2212",0,0 },
  { "msp430g2213",0,0 },
  { "msp430g2221",0,0 },
  { "msp430g2230",0,0 },
  { "msp430g2231",0,0 },
  { "msp430g2232",0,0 },
  { "msp430g2233",0,0 },
  { "msp430g2252",0,0 },
  { "msp430g2253",0,0 },
  { "msp430g2302",0,0 },
  { "msp430g2303",0,0 },
  { "msp430g2312",0,0 },
  { "msp430g2313",0,0 },
  { "msp430g2332",0,0 },
  { "msp430g2333",0,0 },
  { "msp430g2352",0,0 },
  { "msp430g2353",0,0 },
  { "msp430g2402",0,0 },
  { "msp430g2403",0,0 },
  { "msp430g2412",0,0 },
  { "msp430g2413",0,0 },
  { "msp430g2432",0,0 },
  { "msp430g2433",0,0 },
  { "msp430g2444",0,0 },
  { "msp430g2452",0,0 },
  { "msp430g2453",0,0 },
  { "msp430g2513",0,0 },
  { "msp430g2533",0,0 },
  { "msp430g2544",0,0 },
  { "msp430g2553",0,0 },
  { "msp430g2744",0,0 },
  { "msp430g2755",0,0 },
  { "msp430g2855",0,0 },
  { "msp430g2955",0,0 },
  { "msp430i2020",0,2 },
  { "msp430i2021",0,2 },
  { "msp430i2030",0,2 },
  { "msp430i2031",0,2 },
  { "msp430i2040",0,2 },
  { "msp430i2041",0,2 },
  { "msp430i2xxgeneric",0,2 },
  { "msp430l092",0,0 },
  { "msp430p112",0,0 },
  { "msp430p313",0,0 },
  { "msp430p315",0,0 },
  { "msp430p315s",0,0 },
  { "msp430p325",0,0 },
  { "msp430p337",0,1 },
  { "msp430sl5438a",2,8 },
  { "msp430tch5e",0,0 },
  { "msp430xgeneric",2,8 },
  { "rf430f5144",2,8 },
  { "rf430f5155",2,8 },
  { "rf430f5175",2,8 },
  { "rf430frl152h",0,0 },
  { "rf430frl152h_rom",0,0 },
  { "rf430frl153h",0,0 },
  { "rf430frl153h_rom",0,0 },
  { "rf430frl154h",0,0 },
  { "rf430frl154h_rom",0,0 }
};  

/* Generate a C preprocessor symbol based upon the MCU selected by the user.
   If a specific MCU has not been selected then return a generic symbol instead.  */

const char *
msp430_mcu_name (void)
{
  if (target_mcu)
    {
      unsigned int i;
      static char mcu_name [64];

      snprintf (mcu_name, sizeof (mcu_name) - 1, "__%s__", target_mcu);
      for (i = strlen (mcu_name); i--;)
	mcu_name[i] = TOUPPER (mcu_name[i]);
      return mcu_name;
    }

  return msp430x ? "__MSP430XGENERIC__" : "__MSP430GENERIC__";
}

static const char *
hwmult_name (unsigned int val)
{
  switch (val)
    {
    case 0: return "none";
    case 1: return "16-bit";
    case 2: return "16-bit";
    case 4: return "32-bit";
    case 8: return "32-bit (5xx)";
    default: gcc_unreachable ();
    }
}

static void
msp430_option_override (void)
{
  init_machine_status = msp430_init_machine_status;

  if (target_cpu)
    {
      /* gcc/common/config/msp430-common.c will have
	 already canonicalised the string in target_cpu.  */
      if (strcasecmp (target_cpu, "msp430x") == 0)
	msp430x = true;
      else /* target_cpu == "msp430" - already handled by the front end.  */
	msp430x = false;
    }

  if (target_mcu)
    {
      int i;

      /* FIXME: If the array were alpha sorted, we could use a binary search.  */
      for (i = ARRAY_SIZE (msp430_mcu_data); i--;)
	if (strcasecmp (msp430_mcu_data[i].name, target_mcu) == 0)
	  {
	    bool xisa = msp430_mcu_data[i].revision >= 1; 

	    if (msp430_warn_mcu)
	      {
		if (target_cpu&& msp430x != xisa)
		  warning (0, "MCU '%s' supports %s ISA but -mcpu option is set to %s",
			   target_mcu, xisa ? "430X" : "430", msp430x ? "430X" : "430");

		if (msp430_mcu_data[i].hwmpy == 0
		    && msp430_hwmult_type != MSP430_HWMULT_AUTO
		    && msp430_hwmult_type != MSP430_HWMULT_NONE)
		  warning (0, "MCU '%s' does not have hardware multiply support, but -mhwmult is set to %s",
			   target_mcu,
			   msp430_hwmult_type == MSP430_HWMULT_SMALL ? "16-bit"
			   : msp430_hwmult_type == MSP430_HWMULT_LARGE ? "32-bit" : "f5series");
		else if (msp430_hwmult_type == MSP430_HWMULT_SMALL
		    && msp430_mcu_data[i].hwmpy != 1
		    && msp430_mcu_data[i].hwmpy != 2 )
		  warning (0, "MCU '%s' supports %s hardware multiply, but -mhwmult is set to 16-bit",
			   target_mcu, hwmult_name (msp430_mcu_data[i].hwmpy));
		else if (msp430_hwmult_type == MSP430_HWMULT_LARGE && msp430_mcu_data[i].hwmpy != 4)
		  warning (0, "MCU '%s' supports %s hardware multiply, but -mhwmult is set to 32-bit",
			   target_mcu, hwmult_name (msp430_mcu_data[i].hwmpy));
		else if (msp430_hwmult_type == MSP430_HWMULT_F5SERIES && msp430_mcu_data[i].hwmpy != 8)
		  warning (0, "MCU '%s' supports %s hardware multiply, but -mhwmult is set to f5series",
			   target_mcu, hwmult_name (msp430_mcu_data[i].hwmpy));
	      }

	    msp430x = xisa;
	    break;
	  }

      if (i < 0)
	{
	  if (msp430_hwmult_type == MSP430_HWMULT_AUTO)
	    {
	      if (msp430_warn_mcu)
		{
		  if (target_cpu == NULL)
		    warning (0,
			     "Unrecognized MCU name '%s', assuming that it is "
			     "just a MSP430 with no hardware multiply.\n"
			     "Use the -mcpu and -mhwmult options to set "
			     "these explicitly.",
			     target_mcu);
		  else
		    warning (0,
			     "Unrecognized MCU name '%s', assuming that it "
			     "has no hardware multiply.\nUse the -mhwmult "
			     "option to set this explicitly.",
			     target_mcu);
		}

	      msp430_hwmult_type = MSP430_HWMULT_NONE;
	    }
	  else if (target_cpu == NULL)
	    {
	      if (msp430_warn_mcu)
		warning (0,
			 "Unrecognized MCU name '%s', assuming that it just "
			 "supports the MSP430 ISA.\nUse the -mcpu option to "
			 "set the ISA explicitly.",
			 target_mcu);

	      msp430x = false;
	    }
	  else if (msp430_warn_mcu)
	    warning (0, "Unrecognized MCU name '%s'.", target_mcu);
	}
    }

  /* The F5 series are all able to support the 430X ISA.  */
  if (target_cpu == NULL && target_mcu == NULL && msp430_hwmult_type == MSP430_HWMULT_F5SERIES)
    msp430x = true;

  if (TARGET_LARGE && !msp430x)
    error ("-mlarge requires a 430X-compatible -mmcu=");

  if (msp430_code_region == MSP430_REGION_UPPER && ! msp430x)
    error ("-mcode-region=upper requires 430X-compatible cpu");
  if (msp430_data_region == MSP430_REGION_UPPER && ! msp430x)
    error ("-mdata-region=upper requires 430X-compatible cpu");

  if (flag_exceptions || flag_non_call_exceptions
      || flag_unwind_tables || flag_asynchronous_unwind_tables)
    flag_omit_frame_pointer = false;
  else
    flag_omit_frame_pointer = true;

  /* This is a hack to work around a problem with the newlib build
     mechanism.  Newlib always appends CFLAGS to the end of the GCC
     command line and always sets -O2 in CFLAGS.  Thus it is not
     possible to build newlib with -Os enabled.  Until now...  */
  if (TARGET_OPT_SPACE && optimize < 3)
    optimize_size = 1;
}

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P msp430_scalar_mode_supported_p

static bool
msp430_scalar_mode_supported_p (machine_mode m)
{
  if (m == PSImode && msp430x)
    return true;
#if 0
  if (m == TImode)
    return true;
#endif
  return default_scalar_mode_supported_p (m);
}



/* Storage Layout */

#undef  TARGET_MS_BITFIELD_LAYOUT_P
#define TARGET_MS_BITFIELD_LAYOUT_P msp430_ms_bitfield_layout_p

bool
msp430_ms_bitfield_layout_p (const_tree record_type ATTRIBUTE_UNUSED)
{
  return false;
}



/* Register Usage */

/* Implements HARD_REGNO_NREGS.  MSP430X registers can hold a single
   PSImode value, but not an SImode value.  */
int
msp430_hard_regno_nregs (int regno ATTRIBUTE_UNUSED,
			 machine_mode mode)
{
  if (mode == PSImode && msp430x)
    return 1;
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)
	  / UNITS_PER_WORD);
}

/* Implements HARD_REGNO_NREGS_HAS_PADDING.  */
int
msp430_hard_regno_nregs_has_padding (int regno ATTRIBUTE_UNUSED,
				     machine_mode mode)
{
  if (mode == PSImode && msp430x)
    return 1;
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)
	  / UNITS_PER_WORD);
}

/* Implements HARD_REGNO_NREGS_WITH_PADDING.  */
int
msp430_hard_regno_nregs_with_padding (int regno ATTRIBUTE_UNUSED,
				     machine_mode mode)
{
  if (mode == PSImode)
    return 2;
  return msp430_hard_regno_nregs (regno, mode);
}

/* Implements HARD_REGNO_MODE_OK.  */
int
msp430_hard_regno_mode_ok (int regno ATTRIBUTE_UNUSED,
			   machine_mode mode)
{
  return regno <= (ARG_POINTER_REGNUM - msp430_hard_regno_nregs (regno, mode));
}

/* Implements MODES_TIEABLE_P.  */
bool
msp430_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if ((mode1 == PSImode || mode2 == SImode)
      || (mode1 == SImode || mode2 == PSImode))
    return false;

  return ((GET_MODE_CLASS (mode1) == MODE_FLOAT
	   || GET_MODE_CLASS (mode1) == MODE_COMPLEX_FLOAT)
	  == (GET_MODE_CLASS (mode2) == MODE_FLOAT
	      || GET_MODE_CLASS (mode2) == MODE_COMPLEX_FLOAT));
}

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED msp430_frame_pointer_required

static bool
msp430_frame_pointer_required (void)
{
  return false;
}

#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE		msp430_can_eliminate

static bool
msp430_can_eliminate (const int from_reg ATTRIBUTE_UNUSED,
		      const int to_reg ATTRIBUTE_UNUSED)
{
  return true;
}

/* Implements INITIAL_ELIMINATION_OFFSET.  */
int
msp430_initial_elimination_offset (int from, int to)
{
  int rv = 0; /* As if arg to arg.  */

  msp430_compute_frame_info ();

  switch (to)
    {
    case STACK_POINTER_REGNUM:
      rv += cfun->machine->framesize_outgoing;
      rv += cfun->machine->framesize_locals;
      /* Fall through.  */
    case FRAME_POINTER_REGNUM:
      rv += cfun->machine->framesize_regs;
      /* Allow for the saved return address.  */
      rv += (TARGET_LARGE ? 4 : 2);
      /* NB/ No need to allow for crtl->args.pretend_args_size.
         GCC does that for us.  */
      break;
    default:
      gcc_unreachable ();
    }

  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      /* Allow for the fall through above.  */
      rv -= (TARGET_LARGE ? 4 : 2);
      rv -= cfun->machine->framesize_regs;
    case ARG_POINTER_REGNUM:
      break;
    default:
      gcc_unreachable ();
    }

  return rv;
}

/* Named Address Space support */


/* Return the appropriate mode for a named address pointer.  */
#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE msp430_addr_space_pointer_mode
#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE msp430_addr_space_pointer_mode

static scalar_int_mode
msp430_addr_space_pointer_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    default:
    case ADDR_SPACE_GENERIC:
      return Pmode;
    case ADDR_SPACE_NEAR:
      return HImode;
    case ADDR_SPACE_FAR:
      return PSImode;
    }
}

/* Function pointers are stored in unwind_word sized
   variables, so make sure that unwind_word is big enough.  */
#undef  TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE msp430_unwind_word_mode

static scalar_int_mode
msp430_unwind_word_mode (void)
{
  /* This needs to match msp430_init_dwarf_reg_sizes_extra (below).  */
  return msp430x ? PSImode : HImode;
}

/* Determine if one named address space is a subset of another.  */
#undef  TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P msp430_addr_space_subset_p
static bool
msp430_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  if (subset == superset)
    return true;
  else
    return (subset != ADDR_SPACE_FAR && superset == ADDR_SPACE_FAR);
}

#undef  TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT msp430_addr_space_convert
/* Convert from one address space to another.  */
static rtx
msp430_addr_space_convert (rtx op, tree from_type, tree to_type)
{
  addr_space_t from_as = TYPE_ADDR_SPACE (TREE_TYPE (from_type));
  addr_space_t to_as = TYPE_ADDR_SPACE (TREE_TYPE (to_type));
  rtx result;

  if (to_as != ADDR_SPACE_FAR && from_as == ADDR_SPACE_FAR)
    {
      /* This is unpredictable, as we're truncating off usable address
	 bits.  */

      if (CONSTANT_P (op))
	return gen_rtx_CONST (HImode, op);

      result = gen_reg_rtx (HImode);
      emit_insn (gen_truncpsihi2 (result, op));
      return result;
    }
  else if (to_as == ADDR_SPACE_FAR && from_as != ADDR_SPACE_FAR)
    {
      /* This always works.  */

      if (CONSTANT_P (op))
	return gen_rtx_CONST (PSImode, op);

      result = gen_reg_rtx (PSImode);
      emit_insn (gen_zero_extendhipsi2 (result, op));
      return result;
    }
  else
    gcc_unreachable ();
}

/* Stack Layout and Calling Conventions.  */

/* For each function, we list the gcc version and the TI version on
   each line, where we're converting the function names.  */
static char const * const special_convention_function_names [] =
{
  "__muldi3", "__mspabi_mpyll",
  "__udivdi3", "__mspabi_divull",
  "__umoddi3", "__mspabi_remull",
  "__divdi3", "__mspabi_divlli",
  "__moddi3", "__mspabi_remlli",
  "__mspabi_srall",
  "__mspabi_srlll",
  "__mspabi_sllll",
  "__adddf3", "__mspabi_addd",
  "__subdf3", "__mspabi_subd",
  "__muldf3", "__mspabi_mpyd",
  "__divdf3", "__mspabi_divd",
  "__mspabi_cmpd",
  NULL
};

/* TRUE if the function passed is a "speical" function.  Special
   functions pass two DImode parameters in registers.  */
static bool
msp430_special_register_convention_p (const char *name)
{
  int i;

  for (i = 0; special_convention_function_names [i]; i++)
    if (! strcmp (name, special_convention_function_names [i]))
      return true;

  return false;
}

#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P msp430_function_value_regno_p

bool
msp430_function_value_regno_p (unsigned int regno)
{
  return regno == 12;
}


#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE msp430_function_value

rtx
msp430_function_value (const_tree ret_type,
		       const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (ret_type), 12);
}

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE msp430_libcall_value

rtx
msp430_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, 12);
}

/* Implements INIT_CUMULATIVE_ARGS.  */
void
msp430_init_cumulative_args (CUMULATIVE_ARGS *ca,
			     tree fntype ATTRIBUTE_UNUSED,
			     rtx libname ATTRIBUTE_UNUSED,
			     tree fndecl ATTRIBUTE_UNUSED,
			     int n_named_args ATTRIBUTE_UNUSED)
{
  const char *fname;
  memset (ca, 0, sizeof(*ca));

  ca->can_split = 1;

  if (fndecl)
    fname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  else if (libname)
    fname = XSTR (libname, 0);
  else
    fname = NULL;

  if (fname && msp430_special_register_convention_p (fname))
    ca->special_p = 1;
}

/* Helper function for argument passing; this function is the common
   code that determines where an argument will be passed.  */
static void
msp430_evaluate_arg (cumulative_args_t cap,
		     machine_mode mode,
		     const_tree type ATTRIBUTE_UNUSED,
		     bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);
  int nregs = GET_MODE_SIZE (mode);
  int i;

  ca->reg_count = 0;
  ca->mem_count = 0;

  if (!named)
    return;

  if (mode == PSImode)
    nregs = 1;
  else
    nregs = (nregs + 1) / 2;

  if (ca->special_p)
    {
      /* Function is passed two DImode operands, in R8:R11 and
	 R12:15.  */
      ca->start_reg = 8;
      ca->reg_count = 4;
      return;
    }

  switch (nregs)
    {
    case 1:
      for (i = 0; i < 4; i++)
	if (! ca->reg_used [i])
	  {
	    ca->reg_count = 1;
	    ca->start_reg = CA_FIRST_REG + i;
	    return;
	  }
      break;
    case 2:
      for (i = 0; i < 3; i++)
	if (! ca->reg_used [i] && ! ca->reg_used [i + 1])
	  {
	    ca->reg_count = 2;
	    ca->start_reg = CA_FIRST_REG + i;
	    return;
	  }
      if (! ca->reg_used [3] && ca->can_split)
	{
	  ca->reg_count = 1;
	  ca->mem_count = 2;
	  ca->start_reg = CA_FIRST_REG + 3;
	  return;
	}
      break;
    case 3:
    case 4:
      ca->can_split = 0;
      if (! ca->reg_used [0]
	  && ! ca->reg_used [1]
	  && ! ca->reg_used [2]
	  && ! ca->reg_used [3])
	{
	  ca->reg_count = 4;
	  ca->start_reg = CA_FIRST_REG;
	  return;
	}
      break;
    }
}

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES msp430_promote_prototypes

bool
msp430_promote_prototypes (const_tree fntype ATTRIBUTE_UNUSED)
{
  return false;
}

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG msp430_function_arg

rtx
msp430_function_arg (cumulative_args_t cap,
		     machine_mode mode,
		     const_tree type,
		     bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);

  msp430_evaluate_arg (cap, mode, type, named);

  if (ca->reg_count)
    return gen_rtx_REG (mode, ca->start_reg);

  return 0;
}

#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES msp430_arg_partial_bytes

int
msp430_arg_partial_bytes (cumulative_args_t cap,
			  machine_mode mode,
			  tree type,
			  bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);

  msp430_evaluate_arg (cap, mode, type, named);

  if (ca->reg_count && ca->mem_count)
    return ca->reg_count * UNITS_PER_WORD;

  return 0;
}

#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE msp430_pass_by_reference

static bool
msp430_pass_by_reference (cumulative_args_t cap ATTRIBUTE_UNUSED,
			  machine_mode mode,
			  const_tree type,
			  bool named ATTRIBUTE_UNUSED)
{
  return (mode == BLKmode
	  || (type && TREE_CODE (type) == RECORD_TYPE)
	  || (type && TREE_CODE (type) == UNION_TYPE));
}

#undef  TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES msp430_callee_copies

static bool
msp430_callee_copies (cumulative_args_t cap ATTRIBUTE_UNUSED,
                     machine_mode mode ATTRIBUTE_UNUSED,
                     const_tree type ATTRIBUTE_UNUSED,
                     bool named ATTRIBUTE_UNUSED)
{
  return true;
}

#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE msp430_function_arg_advance

void
msp430_function_arg_advance (cumulative_args_t cap,
			     machine_mode mode,
			     const_tree type,
			     bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);
  int i;

  msp430_evaluate_arg (cap, mode, type, named);

  if (ca->start_reg >= CA_FIRST_REG)
    for (i = 0; i < ca->reg_count; i ++)
      ca->reg_used [i + ca->start_reg - CA_FIRST_REG] = 1;

  ca->special_p = 0;
}

#undef  TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY msp430_function_arg_boundary

static unsigned int
msp430_function_arg_boundary (machine_mode mode, const_tree type)
{
  if (mode == BLKmode
      && int_size_in_bytes (type) > 1)
    return 16;
  if (GET_MODE_BITSIZE (mode) > 8)
    return 16;
  return 8;
}

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY msp430_return_in_memory

static bool
msp430_return_in_memory (const_tree ret_type, const_tree fntype ATTRIBUTE_UNUSED)
{
  machine_mode mode = TYPE_MODE (ret_type);

  if (mode == BLKmode
      || (fntype && TREE_CODE (TREE_TYPE (fntype)) == RECORD_TYPE)
      || (fntype && TREE_CODE (TREE_TYPE (fntype)) == UNION_TYPE))
    return true;

  if (GET_MODE_SIZE (mode) > 8)
    return true;

  return false;
}

#undef  TARGET_GET_RAW_ARG_MODE
#define TARGET_GET_RAW_ARG_MODE msp430_get_raw_arg_mode

static machine_mode
msp430_get_raw_arg_mode (int regno)
{
  return (regno == ARG_POINTER_REGNUM) ? VOIDmode : Pmode;
}

#undef  TARGET_GET_RAW_RESULT_MODE
#define TARGET_GET_RAW_RESULT_MODE msp430_get_raw_result_mode

static machine_mode
msp430_get_raw_result_mode (int regno ATTRIBUTE_UNUSED)
{
  return Pmode;
}

#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR msp430_gimplify_va_arg_expr

#include "gimplify.h"

static tree
msp430_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			  gimple_seq *post_p)
{
  tree addr, t, type_size, rounded_size, valist_tmp;
  unsigned HOST_WIDE_INT align, boundary;
  bool indirect;

  indirect = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect)
    type = build_pointer_type (type);

  align = PARM_BOUNDARY / BITS_PER_UNIT;
  boundary = targetm.calls.function_arg_boundary (TYPE_MODE (type), type);

  /* When we align parameter on stack for caller, if the parameter
     alignment is beyond MAX_SUPPORTED_STACK_ALIGNMENT, it will be
     aligned at MAX_SUPPORTED_STACK_ALIGNMENT.  We will match callee
     here with caller.  */
  if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
    boundary = MAX_SUPPORTED_STACK_ALIGNMENT;

  boundary /= BITS_PER_UNIT;

  /* Hoist the valist value into a temporary for the moment.  */
  valist_tmp = get_initialized_tmp_var (valist, pre_p, NULL);

  /* va_list pointer is aligned to PARM_BOUNDARY.  If argument actually
     requires greater alignment, we must perform dynamic alignment.  */
  if (boundary > align
      && !integer_zerop (TYPE_SIZE (type)))
    {
      /* FIXME: This is where this function diverts from targhooks.c:
	 std_gimplify_va_arg_expr().  It works, but I do not know why...  */
      if (! POINTER_TYPE_P (type))
	{
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		      fold_build_pointer_plus_hwi (valist_tmp, boundary - 1));
	  gimplify_and_add (t, pre_p);

	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		      fold_build2 (BIT_AND_EXPR, TREE_TYPE (valist),
				   valist_tmp,
				   build_int_cst (TREE_TYPE (valist), -boundary)));
	  gimplify_and_add (t, pre_p);
	}
    }
  else
    boundary = align;

  /* If the actual alignment is less than the alignment of the type,
     adjust the type accordingly so that we don't assume strict alignment
     when dereferencing the pointer.  */
  boundary *= BITS_PER_UNIT;
  if (boundary < TYPE_ALIGN (type))
    {
      type = build_variant_type_copy (type);
      SET_TYPE_ALIGN (type, boundary);
    }

  /* Compute the rounded size of the type.  */
  type_size = size_in_bytes (type);
  rounded_size = round_up (type_size, align);

  /* Reduce rounded_size so it's sharable with the postqueue.  */
  gimplify_expr (&rounded_size, pre_p, post_p, is_gimple_val, fb_rvalue);

  /* Get AP.  */
  addr = valist_tmp;

  /* Compute new value for AP.  */
  t = fold_build_pointer_plus (valist_tmp, rounded_size);
  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
  gimplify_and_add (t, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (indirect)
    addr = build_va_arg_indirect_ref (addr);

  addr = build_va_arg_indirect_ref (addr);

  return addr;
}

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

/* Addressing Modes */

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P msp430_legitimate_address_p

static bool
reg_ok_for_addr (rtx r, bool strict)
{
  int rn = REGNO (r);

  if (strict && rn >= FIRST_PSEUDO_REGISTER)
    rn = reg_renumber [rn];
  if (strict && 0 <= rn && rn < FIRST_PSEUDO_REGISTER)
    return true;
  if (!strict)
    return true;
  return false;
}

bool
msp430_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			     rtx x ATTRIBUTE_UNUSED,
			     bool strict ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (x))
    {
    case MEM:
      return false;

    case PLUS:
      if (REG_P (XEXP (x, 0)))
	{
	  if (GET_MODE (x) != GET_MODE (XEXP (x, 0)))
	    return false;
	  if (!reg_ok_for_addr (XEXP (x, 0), strict))
	    return false;
	  switch (GET_CODE (XEXP (x, 1)))
	    {
	    case CONST:
	    case SYMBOL_REF:
	    case CONST_INT:
	      return true;
	    default:
	      return false;
	    }
	}
      return false;

    case REG:
      if (!reg_ok_for_addr (x, strict))
	return false;
      /* FALLTHRU */
    case CONST:
    case SYMBOL_REF:
    case CONST_INT:
      return true;

    default:
      return false;
    }
}

#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P msp430_addr_space_legitimate_address_p

bool
msp430_addr_space_legitimate_address_p (machine_mode mode,
					rtx x,
					bool strict,
					addr_space_t as ATTRIBUTE_UNUSED)
{
  return msp430_legitimate_address_p (mode, x, strict);
}

#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER msp430_asm_integer
static bool
msp430_asm_integer (rtx x, unsigned int size, int aligned_p)
{
  int c = GET_CODE (x);

  if (size == 3 && GET_MODE (x) == PSImode)
    size = 4;

  switch (size)
    {
    case 4:
      if (c == SYMBOL_REF || c == CONST || c == LABEL_REF || c == CONST_INT
	  || c == PLUS || c == MINUS)
	{
	  fprintf (asm_out_file, "\t.long\t");
	  output_addr_const (asm_out_file, x);
	  fputc ('\n', asm_out_file);
	  return true;
	}
      break;
    }
  return default_assemble_integer (x, size, aligned_p);
}

#undef  TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA msp430_asm_output_addr_const_extra
static bool
msp430_asm_output_addr_const_extra (FILE *file ATTRIBUTE_UNUSED, rtx x)
{
  debug_rtx(x);
  return false;
}

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P msp430_legitimate_constant

static bool
msp430_legitimate_constant (machine_mode mode, rtx x)
{
  return ! CONST_INT_P (x)
    || mode != PSImode
    /* GCC does not know the width of the PSImode, so make
       sure that it does not try to use a constant value that
       is out of range.  */
    || (INTVAL (x) < (1 << 20) && INTVAL (x) >= (HOST_WIDE_INT)(HOST_WIDE_INT_M1U << 20));
}


#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS msp430_rtx_costs

static bool msp430_rtx_costs (rtx	   x ATTRIBUTE_UNUSED,
			      machine_mode mode,
			      int	   outer_code ATTRIBUTE_UNUSED,
			      int	   opno ATTRIBUTE_UNUSED,
			      int *	   total,
			      bool	   speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  switch (code)
    {
    case SIGN_EXTEND:
      if (mode == SImode && outer_code == SET)
	{
	  *total = COSTS_N_INSNS (4);
	  return true;
	}
      break;
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (!msp430x)
	{
	  *total = COSTS_N_INSNS (100);
	  return true;
	}
      break;
    }
  return false;
}

/* Function Entry and Exit */

/* The MSP430 call frame looks like this:

   <higher addresses>
   +--------------------+
   |                    |
   | Stack Arguments    |
   |                    |
   +--------------------+ <-- "arg pointer"
   |                    |
   | PC from call       |  (2 bytes for 430, 4 for TARGET_LARGE)
   |                    |
   +--------------------+
   | SR if this func has|
   | been called via an |
   | interrupt.         |
   +--------------------+  <-- SP before prologue, also AP
   |                    |
   | Saved Regs         |  (2 bytes per reg for 430, 4 per for TARGET_LARGE)
   |                    |
   +--------------------+  <-- "frame pointer"
   |                    |
   | Locals             |
   |                    |
   +--------------------+
   |                    |
   | Outgoing Args      |
   |                    |
   +--------------------+  <-- SP during function
   <lower addresses>

*/

/* We use this to wrap all emitted insns in the prologue, so they get
   the "frame-related" (/f) flag set.  */
static rtx
F (rtx x)
{
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* This is the one spot that decides if a register is to be saved and
   restored in the prologue/epilogue.  */
static bool
msp430_preserve_reg_p (int regno)
{
  /* PC, SP, SR, and the constant generator.  */
  if (regno <= 3)
    return false;

  /* FIXME: add interrupt, EH, etc.  */
  if (crtl->calls_eh_return)
    return true;

  /* Shouldn't be more than the above, but just in case...  */
  if (fixed_regs [regno])
    return false;

  /* Interrupt handlers save all registers they use, even
     ones which are call saved.  If they call other functions
     then *every* register is saved.  */
  if (msp430_is_interrupt_func ())
    return ! crtl->is_leaf || df_regs_ever_live_p (regno);

  if (!call_used_regs [regno]
      && df_regs_ever_live_p (regno))
    return true;

  return false;
}

/* Compute all the frame-related fields in our machine_function
   structure.  */
static void
msp430_compute_frame_info (void)
{
  int i;

  cfun->machine->computed = 1;
  cfun->machine->framesize_regs = 0;
  cfun->machine->framesize_locals = get_frame_size ();
  cfun->machine->framesize_outgoing = crtl->outgoing_args_size;

  for (i = 0; i < ARG_POINTER_REGNUM; i ++)
    if (msp430_preserve_reg_p (i))
      {
	cfun->machine->need_to_save [i] = 1;
	cfun->machine->framesize_regs += (TARGET_LARGE ? 4 : 2);
      }
    else
      cfun->machine->need_to_save [i] = 0;

  if ((cfun->machine->framesize_locals + cfun->machine->framesize_outgoing) & 1)
    cfun->machine->framesize_locals ++;

  cfun->machine->framesize = (cfun->machine->framesize_regs
			      + cfun->machine->framesize_locals
			      + cfun->machine->framesize_outgoing);
}

/* Attribute Handling.  */

const char * const  ATTR_INTR   = "interrupt";
const char * const  ATTR_WAKEUP = "wakeup";
const char * const  ATTR_NAKED  = "naked";
const char * const  ATTR_REENT  = "reentrant";
const char * const  ATTR_CRIT   = "critical";
const char * const  ATTR_LOWER  = "lower";
const char * const  ATTR_UPPER  = "upper";
const char * const  ATTR_EITHER = "either";
const char * const  ATTR_NOINIT = "noinit";
const char * const  ATTR_PERSIST = "persistent";

static inline bool
has_attr (const char * attr, tree decl)
{
  if (decl == NULL_TREE)
    return false;
  return lookup_attribute (attr, DECL_ATTRIBUTES (decl)) != NULL_TREE;
}

static bool
is_interrupt_func (tree decl = current_function_decl)
{
  return has_attr (ATTR_INTR, decl);
}

/* Returns true if the current function has the "interrupt" attribute.  */

bool
msp430_is_interrupt_func (void)
{
  return is_interrupt_func (current_function_decl);
}

static bool
is_wakeup_func (tree decl = current_function_decl)
{
  return is_interrupt_func (decl) && has_attr (ATTR_WAKEUP, decl);
}

static inline bool
is_naked_func (tree decl = current_function_decl)
{
  return has_attr (ATTR_NAKED, decl);
}

static inline bool
is_reentrant_func (tree decl = current_function_decl)
{
  return has_attr (ATTR_REENT, decl);
}

static inline bool
is_critical_func (tree decl = current_function_decl)
{
  return has_attr (ATTR_CRIT, decl);
}

static bool
has_section_name (const char * name, tree decl = current_function_decl)
{
  if (decl == NULL_TREE)
    return false;
  return (DECL_SECTION_NAME (decl)
    && (strcmp (name, DECL_SECTION_NAME (decl)) == 0));
}

#undef  TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS	msp430_allocate_stack_slots_for_args

static bool
msp430_allocate_stack_slots_for_args (void)
{
  /* Naked functions should not allocate stack slots for arguments.  */
  return ! is_naked_func ();
}

/* Verify MSP430 specific attributes.  */
#define TREE_NAME_EQ(NAME, STR) (strcmp (IDENTIFIER_POINTER (NAME), (STR)) == 0)

static tree
msp430_attr (tree * node,
	     tree   name,
	     tree   args,
	     int    flags ATTRIBUTE_UNUSED,
	     bool * no_add_attrs)
{
  gcc_assert (DECL_P (* node));

  if (args != NULL)
    {
      /* Only the interrupt attribute takes an argument.  */
      gcc_assert (TREE_NAME_EQ (name, ATTR_INTR));

      tree value = TREE_VALUE (args);

      switch (TREE_CODE (value))
	{
	case STRING_CST:
	  if (   strcmp (TREE_STRING_POINTER (value), "reset")
	      && strcmp (TREE_STRING_POINTER (value), "nmi")
	      && strcmp (TREE_STRING_POINTER (value), "watchdog"))
	    /* Allow the attribute to be added - the linker script
	       being used may still recognise this name.  */
	    warning (OPT_Wattributes,
		     "unrecognized interrupt vector argument of %qE attribute",
		     name);
	  break;

	case INTEGER_CST:
	  if (wi::gtu_p (value, 63))
	    /* Allow the attribute to be added - the linker script
	       being used may still recognise this value.  */
	    warning (OPT_Wattributes,
		     "numeric argument of %qE attribute must be in range 0..63",
		     name);
	  break;

	default:
	  warning (OPT_Wattributes,
		   "argument of %qE attribute is not a string constant or number",
		   name);
	  *no_add_attrs = true;
	  break;
	}
    }

  const char * message = NULL;

  if (TREE_CODE (* node) != FUNCTION_DECL)
    {
      message = "%qE attribute only applies to functions";
    }
  else if (TREE_NAME_EQ (name, ATTR_INTR))
    {
      if (TREE_CODE (TREE_TYPE (* node)) == FUNCTION_TYPE
	  && ! VOID_TYPE_P (TREE_TYPE (TREE_TYPE (* node))))
	message = "interrupt handlers must be void";

      if (! TREE_PUBLIC (* node))
	message = "interrupt handlers cannot be static";

      /* Ensure interrupt handlers never get optimised out.  */
      TREE_USED (* node) = 1;
      DECL_PRESERVE_P (* node) = 1;
    }
  else if (TREE_NAME_EQ (name, ATTR_REENT))
    {
      if (is_naked_func (* node))
	message = "naked functions cannot be reentrant";
      else if (is_critical_func (* node))
	message = "critical functions cannot be reentrant";
    }
  else if (TREE_NAME_EQ (name, ATTR_CRIT))
    {
      if (is_naked_func (* node))
	message = "naked functions cannot be critical";
      else if (is_reentrant_func (* node))
	message = "reentrant functions cannot be critical";
    }
  else if (TREE_NAME_EQ (name, ATTR_NAKED))
    {
      if (is_critical_func (* node))
	message = "critical functions cannot be naked";
      else if (is_reentrant_func (* node))
	message = "reentrant functions cannot be naked";
    }

  if (message)
    {
      warning (OPT_Wattributes, message, name);
      * no_add_attrs = true;
    }
    
  return NULL_TREE;
}

static tree
msp430_section_attr (tree * node,
		     tree   name,
		     tree   args,
		     int    flags ATTRIBUTE_UNUSED,
		     bool * no_add_attrs ATTRIBUTE_UNUSED)
{
  gcc_assert (DECL_P (* node));
  gcc_assert (args == NULL);

  const char * message = NULL;

  if (TREE_NAME_EQ (name, ATTR_UPPER))
    {
      if (has_attr (ATTR_LOWER, * node))
	message = "already marked with 'lower' attribute";
      else if (has_attr (ATTR_EITHER, * node))
	message = "already marked with 'either' attribute";
      else if (! msp430x)
	message = "upper attribute needs a 430X cpu";
    }
  else if (TREE_NAME_EQ (name, ATTR_LOWER))
    {
      if (has_attr (ATTR_UPPER, * node))
	message = "already marked with 'upper' attribute";
      else if (has_attr (ATTR_EITHER, * node))
	message = "already marked with 'either' attribute";
    }
  else
    {
      gcc_assert (TREE_NAME_EQ (name, ATTR_EITHER));

      if (has_attr (ATTR_LOWER, * node))
	message = "already marked with 'lower' attribute";
      else if (has_attr (ATTR_UPPER, * node))
	message = "already marked with 'upper' attribute";
    }

  if (message)
    {
      warning (OPT_Wattributes, message, name);
      * no_add_attrs = true;
    }
    
  return NULL_TREE;
}

static tree
msp430_data_attr (tree * node,
		  tree   name,
		  tree   args,
		  int    flags ATTRIBUTE_UNUSED,
		  bool * no_add_attrs ATTRIBUTE_UNUSED)
{
  const char * message = NULL;

  gcc_assert (DECL_P (* node));
  gcc_assert (args == NULL);

  if (TREE_CODE (* node) != VAR_DECL)
    message = G_("%qE attribute only applies to variables");

  /* Check that it's possible for the variable to have a section.  */
  if ((TREE_STATIC (* node) || DECL_EXTERNAL (* node) || in_lto_p)
      && DECL_SECTION_NAME (* node))
    message = G_("%qE attribute cannot be applied to variables with specific sections");

  if (!message && TREE_NAME_EQ (name, ATTR_PERSIST) && !TREE_STATIC (* node)
      && !TREE_PUBLIC (* node) && !DECL_EXTERNAL (* node))
    message = G_("%qE attribute has no effect on automatic variables");

  /* It's not clear if there is anything that can be set here to prevent the
     front end placing the variable before the back end can handle it, in a
     similar way to how DECL_COMMON is used below.
     So just place the variable in the .persistent section now.  */
  if ((TREE_STATIC (* node) || DECL_EXTERNAL (* node) || in_lto_p)
      && TREE_NAME_EQ (name, ATTR_PERSIST))
    set_decl_section_name (* node, ".persistent");

  /* If this var is thought to be common, then change this.  Common variables
     are assigned to sections before the backend has a chance to process them.  */
  if (DECL_COMMON (* node))
    DECL_COMMON (* node) = 0;

  if (message)
    {
      warning (OPT_Wattributes, message, name);
      * no_add_attrs = true;
    }
    
  return NULL_TREE;
}


#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE		msp430_attribute_table

/* Table of MSP430-specific attributes.  */
const struct attribute_spec msp430_attribute_table[] =
{
  /* Name        min_num_args     type_req,             affects_type_identity
                      max_num_args,     fn_type_req
                          decl_req               handler.  */
  { ATTR_INTR,        0, 1, true,  false, false, msp430_attr, false },
  { ATTR_NAKED,       0, 0, true,  false, false, msp430_attr, false },
  { ATTR_REENT,       0, 0, true,  false, false, msp430_attr, false },
  { ATTR_CRIT,        0, 0, true,  false, false, msp430_attr, false },
  { ATTR_WAKEUP,      0, 0, true,  false, false, msp430_attr, false },

  { ATTR_LOWER,       0, 0, true,  false, false, msp430_section_attr, false },
  { ATTR_UPPER,       0, 0, true,  false, false, msp430_section_attr, false },
  { ATTR_EITHER,      0, 0, true,  false, false, msp430_section_attr, false },

  { ATTR_NOINIT,      0, 0, true,  false, false, msp430_data_attr, false },
  { ATTR_PERSIST,     0, 0, true,  false, false, msp430_data_attr, false },

  { NULL,             0, 0, false, false, false, NULL, false }
};

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE	msp430_start_function

static void
msp430_start_function (FILE *outfile)
{
  int r, n;

  fprintf (outfile, "; start of function\n");

  if (DECL_ATTRIBUTES (current_function_decl) != NULL_TREE)
    {
      fprintf (outfile, "; attributes: ");
      if (is_naked_func ())
	fprintf (outfile, "naked ");
      if (msp430_is_interrupt_func ())
	fprintf (outfile, "interrupt ");
      if (is_reentrant_func ())
	fprintf (outfile, "reentrant ");
      if (is_critical_func ())
	fprintf (outfile, "critical ");
      if (is_wakeup_func ())
	fprintf (outfile, "wakeup ");
      fprintf (outfile, "\n");
    }

  fprintf (outfile, "; framesize_regs:     %d\n", cfun->machine->framesize_regs);
  fprintf (outfile, "; framesize_locals:   %d\n", cfun->machine->framesize_locals);
  fprintf (outfile, "; framesize_outgoing: %d\n", cfun->machine->framesize_outgoing);
  fprintf (outfile, "; framesize:          %d\n", cfun->machine->framesize);
  fprintf (outfile, "; elim ap -> fp       %d\n", msp430_initial_elimination_offset (ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM));
  fprintf (outfile, "; elim fp -> sp       %d\n", msp430_initial_elimination_offset (FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM));

  n = 0;
  fprintf (outfile, "; saved regs:");
  for (r = 0; r < ARG_POINTER_REGNUM; r++)
    if (cfun->machine->need_to_save [r])
      {
	fprintf (outfile, " %s", reg_names [r]);
	n = 1;
      }
  if (n == 0)
    fprintf (outfile, "(none)");
  fprintf (outfile, "\n");
}

/* Common code to change the stack pointer.  */
static void
increment_stack (HOST_WIDE_INT amount)
{
  rtx inc;
  rtx sp =  stack_pointer_rtx;

  if (amount == 0)
    return;

  if (amount < 0)
    {
      inc = GEN_INT (- amount);
      if (TARGET_LARGE)
	F (emit_insn (gen_subpsi3 (sp, sp, inc)));
      else
	F (emit_insn (gen_subhi3 (sp, sp, inc)));
    }
  else
    {
      inc = GEN_INT (amount);
      if (TARGET_LARGE)
	emit_insn (gen_addpsi3 (sp, sp, inc));
      else
	emit_insn (gen_addhi3 (sp, sp, inc));
    }
}

void
msp430_start_function (FILE *file, const char *name, tree decl)
{
  tree int_attr;

  int_attr = lookup_attribute ("interrupt", DECL_ATTRIBUTES (decl));
  if (int_attr != NULL_TREE)
    {
      tree intr_vector = TREE_VALUE (int_attr);

      if (intr_vector != NULL_TREE)
	{
	  char buf[101];

	  /* Interrupt vector sections should be unique, but use of weak
	     functions implies multiple definitions.  */
	  if (DECL_WEAK (decl))
	    {
	      error ("argument to interrupt attribute is unsupported for weak functions");
	    }

	  intr_vector = TREE_VALUE (intr_vector);

	  /* The interrupt attribute has a vector value.  Turn this into a
	     section name, switch to that section and put the address of
	     the current function into that vector slot.  Note msp430_attr()
	     has already verified the vector name for us.  */
	  if (TREE_CODE (intr_vector) == STRING_CST)
	    sprintf (buf, "__interrupt_vector_%.80s",
		     TREE_STRING_POINTER (intr_vector));
	  else /* TREE_CODE (intr_vector) == INTEGER_CST */
	    sprintf (buf, "__interrupt_vector_%u",
		     (unsigned int) TREE_INT_CST_LOW (intr_vector));

	  switch_to_section (get_section (buf, SECTION_CODE, decl));
	  fputs ("\t.word\t", file);
	  assemble_name (file, name);
	  fputc ('\n', file);
	  fputc ('\t', file);
	}
    }

  switch_to_section (function_section (decl));
  ASM_OUTPUT_TYPE_DIRECTIVE(file, name, "function");
  ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);
}

static const char * const lower_prefix = ".lower";
static const char * const upper_prefix = ".upper";
static const char * const either_prefix = ".either";

/* Generate a prefix for a section name, based upon
   the region into which the object should be placed.  */

static const char *
gen_prefix (tree decl)
{
  if (DECL_ONE_ONLY (decl))
    return NULL;

  /* If the user has specified a particular section then do not use any prefix.  */
  if (has_attr ("section", decl))
    return NULL;

  /* If the function has been put in the .lowtext section (because it is an
     interrupt handler, and the large memory model is used), then do not add
     any prefixes.  */
  if (has_section_name (".lowtext", decl))
    return NULL;

  /* If the object has __attribute__((lower)) then use the ".lower." prefix.  */
  if (has_attr (ATTR_LOWER, decl))
    return lower_prefix;

  /* If we are compiling for the MSP430 then we do not support the upper region.  */
  if (! msp430x)
    return NULL;

  if (has_attr (ATTR_UPPER, decl))
    return upper_prefix;

  if (has_attr (ATTR_EITHER, decl))
    return either_prefix;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (msp430_code_region == MSP430_REGION_LOWER)
	return lower_prefix;

      if (msp430_code_region == MSP430_REGION_UPPER)
	return upper_prefix;

      if (msp430_code_region == MSP430_REGION_EITHER)
	return either_prefix;
    }
  else
    {
      if (msp430_data_region == MSP430_REGION_LOWER)
	return lower_prefix;

      if (msp430_data_region == MSP430_REGION_UPPER)
	return upper_prefix;

      if (msp430_data_region == MSP430_REGION_EITHER)
	return either_prefix;
    }

  return NULL;
}

static section * noinit_section;
static section * persist_section;

#undef  TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS msp430_init_sections

static void
msp430_init_sections (void)
{
  noinit_section = get_unnamed_section (0, output_section_asm_op, ".section .noinit,\"aw\"");
  persist_section = get_unnamed_section (0, output_section_asm_op, ".section .persistent,\"aw\"");
}

#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION msp430_select_section

static section *
msp430_select_section (tree decl, int reloc, unsigned HOST_WIDE_INT align)
{
  gcc_assert (decl != NULL_TREE);

  if (TREE_CODE (decl) == STRING_CST
      || TREE_CODE (decl) == CONSTRUCTOR
      || TREE_CODE (decl) == INTEGER_CST
      || TREE_CODE (decl) == VECTOR_CST
      || TREE_CODE (decl) == COMPLEX_CST)
    return default_select_section (decl, reloc, align);
  
  /* In large mode we must make sure that interrupt handlers are put into
     low memory as the vector table only accepts 16-bit addresses.  */
  if (TARGET_LARGE && TREE_CODE (decl) == FUNCTION_DECL && is_interrupt_func (decl))
    return get_section (".lowtext", SECTION_CODE | SECTION_WRITE , decl);

  const char * prefix = gen_prefix (decl);
  if (prefix == NULL)
    {
      if (TREE_CODE (decl) == FUNCTION_DECL)
	return text_section;
      else if (has_attr (ATTR_NOINIT, decl))
	return noinit_section;
      else if (has_attr (ATTR_PERSIST, decl))
	return persist_section;
      else
	return default_select_section (decl, reloc, align);
    }
  
  const char * sec;
  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_TEXT:   sec = ".text";   break;
    case SECCAT_DATA:   sec = ".data";   break;
    case SECCAT_BSS:    sec = ".bss";    break;
    case SECCAT_RODATA: sec = ".rodata"; break;

    case SECCAT_RODATA_MERGE_STR:
    case SECCAT_RODATA_MERGE_STR_INIT:
    case SECCAT_RODATA_MERGE_CONST:
    case SECCAT_SRODATA:
    case SECCAT_DATA_REL:
    case SECCAT_DATA_REL_LOCAL:
    case SECCAT_DATA_REL_RO:
    case SECCAT_DATA_REL_RO_LOCAL:
    case SECCAT_SDATA:
    case SECCAT_SBSS:
    case SECCAT_TDATA:
    case SECCAT_TBSS:
      return default_select_section (decl, reloc, align);

    default:
      gcc_unreachable ();
    }
  
  const char * dec_name = DECL_SECTION_NAME (decl);
  char * name = ACONCAT ((prefix, sec, dec_name, NULL));

  return get_named_section (decl, name, 0);
}

#undef  TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION msp430_function_section

static section *
msp430_function_section (tree decl, enum node_frequency freq, bool startup, bool exit)
{
  const char * name;

  gcc_assert (DECL_SECTION_NAME (decl) != NULL);
  name = DECL_SECTION_NAME (decl);

  const char * prefix = gen_prefix (decl);
  if (prefix == NULL
      || strncmp (name, prefix, strlen (prefix)) == 0)
    return default_function_section (decl, freq, startup, exit);

  name = ACONCAT ((prefix, name, NULL));
  return get_named_section (decl, name, 0);
}

#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS msp430_section_type_flags

unsigned int
msp430_section_type_flags (tree decl, const char * name, int reloc)
{
  if (strncmp (name, lower_prefix, strlen (lower_prefix)) == 0)
    name += strlen (lower_prefix);
  else if (strncmp (name, upper_prefix, strlen (upper_prefix)) == 0)
    name += strlen (upper_prefix);
  else if (strncmp (name, either_prefix, strlen (either_prefix)) == 0)
    name += strlen (either_prefix);
  else if (strcmp (name, ".noinit") == 0)
    return SECTION_WRITE | SECTION_BSS | SECTION_NOTYPE;
  else if (strcmp (name, ".persistent") == 0)
    return SECTION_WRITE | SECTION_NOTYPE;
  
  return default_section_type_flags (decl, name, reloc);
}

#undef  TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION msp430_unique_section

static void
msp430_unique_section (tree decl, int reloc)
{
  gcc_assert (decl != NULL_TREE);

  /* In large mode we must make sure that interrupt handlers are put into
     low memory as the vector table only accepts 16-bit addresses.  */
  if (TARGET_LARGE && TREE_CODE (decl) == FUNCTION_DECL && is_interrupt_func (decl))
    {
      set_decl_section_name (decl, ".lowtext");
      return;
    }

  default_unique_section (decl, reloc);

  const char * prefix;

  if (   TREE_CODE (decl) == STRING_CST
      || TREE_CODE (decl) == CONSTRUCTOR
      || TREE_CODE (decl) == INTEGER_CST
      || TREE_CODE (decl) == VECTOR_CST
      || TREE_CODE (decl) == COMPLEX_CST
      || (prefix = gen_prefix (decl)) == NULL
      )
    return;

  const char * dec_name = DECL_SECTION_NAME (decl);
  char * name = ACONCAT ((prefix, dec_name, NULL));

  set_decl_section_name (decl, name);
}

/* Emit a declaration of a common symbol.
   If a data region is in use then put the symbol into the
   equivalent .bss section instead.  */

void
msp430_output_aligned_decl_common (FILE *                 stream,
				   const tree             decl,
				   const char *           name,
				   unsigned HOST_WIDE_INT size,
				   unsigned int           align)
{
  if (msp430_data_region == MSP430_REGION_ANY)
    {
      fprintf (stream, COMMON_ASM_OP);
      assemble_name (stream, name);
      fprintf (stream, "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",
	       size, align / BITS_PER_UNIT);
    }
  else
    {
      section * sec;

      if (decl)
	sec = msp430_select_section (decl, 0, align);
      else
	switch (msp430_data_region)
	  {
	  case MSP430_REGION_UPPER: sec = get_named_section (NULL, ".upper.bss", 0); break;
	  case MSP430_REGION_LOWER: sec = get_named_section (NULL, ".lower.bss", 0); break;
	  case MSP430_REGION_EITHER: sec = get_named_section (NULL, ".either.bss", 0); break;
	  default:
	    gcc_unreachable ();
	  }
      gcc_assert (sec != NULL);

      switch_to_section (sec);
      ASM_OUTPUT_ALIGN (stream, floor_log2 (align / BITS_PER_UNIT));
      targetm.asm_out.globalize_label (stream, name);
      ASM_WEAKEN_LABEL (stream, name);
      ASM_OUTPUT_LABEL (stream, name);
      ASM_OUTPUT_SKIP (stream, size ? size : 1);
    }
}

bool
msp430_do_not_relax_short_jumps (void)
{
  /* When placing code into "either" low or high memory we do not want the linker
     to grow the size of sections, which it can do if it is encounters a branch to
     a label that is too far away.  So we tell the cbranch patterns to avoid using
     short jumps when there is a chance that the instructions will end up in a low
     section.  */
  return
    msp430_code_region == MSP430_REGION_EITHER
    || msp430_code_region == MSP430_REGION_LOWER
    || has_attr (ATTR_EITHER, current_function_decl)
    || has_attr (ATTR_LOWER, current_function_decl);
}

enum msp430_builtin
{
  MSP430_BUILTIN_BIC_SR,
  MSP430_BUILTIN_BIS_SR,
  MSP430_BUILTIN_DELAY_CYCLES,
  MSP430_BUILTIN_max
};

static GTY(()) tree msp430_builtins [(int) MSP430_BUILTIN_max];

static void
msp430_init_builtins (void)
{
  tree void_ftype_int = build_function_type_list (void_type_node, integer_type_node, NULL);
  tree void_ftype_longlong = build_function_type_list (void_type_node, long_long_integer_type_node, NULL);

  msp430_builtins[MSP430_BUILTIN_BIC_SR] =
    add_builtin_function ( "__bic_SR_register_on_exit", void_ftype_int,
			   MSP430_BUILTIN_BIC_SR, BUILT_IN_MD, NULL, NULL_TREE);

  msp430_builtins[MSP430_BUILTIN_BIS_SR] =
    add_builtin_function ( "__bis_SR_register_on_exit", void_ftype_int,
			   MSP430_BUILTIN_BIS_SR, BUILT_IN_MD, NULL, NULL_TREE);

  msp430_builtins[MSP430_BUILTIN_DELAY_CYCLES] =
    add_builtin_function ( "__delay_cycles", void_ftype_longlong,
			   MSP430_BUILTIN_DELAY_CYCLES, BUILT_IN_MD, NULL, NULL_TREE);
}

static tree
msp430_builtin_decl (unsigned code, bool initialize ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case MSP430_BUILTIN_BIC_SR:
    case MSP430_BUILTIN_BIS_SR:
    case MSP430_BUILTIN_DELAY_CYCLES:
      return msp430_builtins[code];
    default:
      return error_mark_node;
    }
}

/* These constants are really register reads, which are faster than
   regular constants.  */
static int
cg_magic_constant (HOST_WIDE_INT c)
{
  switch (c)
    {
    case 0xffff:
    case -1:
    case 0:
    case 1:
    case 2:
    case 4:
    case 8:
      return 1;
    default:
      return 0;
    }
}

static rtx
msp430_expand_delay_cycles (rtx arg)
{
  HOST_WIDE_INT i, c, n;
  /* extra cycles for MSP430X instructions */
#define CYCX(M,X) (msp430x ? (X) : (M))

  if (GET_CODE (arg) != CONST_INT)
    {
      error ("__delay_cycles() only takes constant arguments");
      return NULL_RTX;
    }

  c = INTVAL (arg);

  if (HOST_BITS_PER_WIDE_INT > 32)
    {
      if (c < 0)
	{
	  error ("__delay_cycles only takes non-negative cycle counts");
	  return NULL_RTX;
	}
    }

  emit_insn (gen_delay_cycles_start (arg));

  /* For 32-bit loops, there's 13(16) + 5(min(x,0x10000) + 6x cycles.  */
  if (c > 3 * 0xffff + CYCX (7, 10))
    {
      n = c;
      /* There's 4 cycles in the short (i>0xffff) loop and 7 in the long (x<=0xffff) loop */
      if (c >= 0x10000 * 7 + CYCX (14, 16))
	{
	  i = 0x10000;
	  c -= CYCX (14, 16) + 7 * 0x10000;
	  i += c / 4;
	  c %= 4;
	  if ((unsigned long long) i > 0xffffffffULL)
	    {
	      error ("__delay_cycles is limited to 32-bit loop counts");
	      return NULL_RTX;
	    }
	}
      else
	{
	  i = (c - CYCX (14, 16)) / 7;
	  c -= CYCX (14, 16) + i * 7;
	}

      if (cg_magic_constant (i & 0xffff))
	c ++;
      if (cg_magic_constant ((i >> 16) & 0xffff))
	c ++;

      if (msp430x)
	emit_insn (gen_delay_cycles_32x (GEN_INT (i), GEN_INT (n - c)));
      else
	emit_insn (gen_delay_cycles_32 (GEN_INT (i), GEN_INT (n - c)));
    }

  /* For 16-bit loops, there's 7(10) + 3x cycles - so the max cycles is 0x30004(7).  */
  if (c > 12)
    {
      n = c;
      i = (c - CYCX (7, 10)) / 3;
      c -= CYCX (7, 10) + i * 3;

      if (cg_magic_constant (i))
	c ++;

      if (msp430x)
	emit_insn (gen_delay_cycles_16x (GEN_INT (i), GEN_INT (n - c)));
      else
	emit_insn (gen_delay_cycles_16 (GEN_INT (i), GEN_INT (n - c)));
    }

  while (c > 1)
    {
      emit_insn (gen_delay_cycles_2 ());
      c -= 2;
    }

  if (c)
    {
      emit_insn (gen_delay_cycles_1 ());
      c -= 1;
    }

  emit_insn (gen_delay_cycles_end (arg));

  return NULL_RTX;
}

static rtx
msp430_expand_builtin (tree exp,
		       rtx target ATTRIBUTE_UNUSED,
		       rtx subtarget ATTRIBUTE_UNUSED,
		       machine_mode mode ATTRIBUTE_UNUSED,
		       int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  rtx arg1 = expand_normal (CALL_EXPR_ARG (exp, 0));

  if (fcode == MSP430_BUILTIN_DELAY_CYCLES)
    return msp430_expand_delay_cycles (arg1);

  if (! msp430_is_interrupt_func ())
    {
      error ("MSP430 builtin functions only work inside interrupt handlers");
      return NULL_RTX;
    }

  if (! REG_P (arg1) && ! CONSTANT_P (arg1))
    arg1 = force_reg (mode, arg1);

  switch (fcode)
    {
    case MSP430_BUILTIN_BIC_SR:  emit_insn (gen_bic_SR (arg1)); break;
    case MSP430_BUILTIN_BIS_SR:  emit_insn (gen_bis_SR (arg1)); break;
    default:
      internal_error ("bad builtin code");
      break;
    }
  return NULL_RTX;
}

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  msp430_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN msp430_expand_builtin

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL   msp430_builtin_decl

void
msp430_expand_prologue (void)
{
  int i, j;
  int fs;
  /* Always use stack_pointer_rtx instead of calling
     rtx_gen_REG ourselves.  Code elsewhere in GCC assumes
     that there is a single rtx representing the stack pointer,
     namely stack_pointer_rtx, and uses == to recognize it.  */
  rtx sp = stack_pointer_rtx;
  rtx p;

  if (is_naked_func ())
    {
      /* We must generate some RTX as thread_prologue_and_epilogue_insns()
	 examines the output of the gen_prologue() function.  */
      emit_insn (gen_rtx_CLOBBER (VOIDmode, GEN_INT (0)));
      return;
    }

  emit_insn (gen_prologue_start_marker ());

  if (is_critical_func ())
    {
      emit_insn (gen_push_intr_state ());
      emit_insn (gen_disable_interrupts ());
    }
  else if (is_reentrant_func ())
    emit_insn (gen_disable_interrupts ());

  if (!cfun->machine->computed)
    msp430_compute_frame_info ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->framesize;

  if (crtl->args.pretend_args_size)
    {
      rtx note;

      gcc_assert (crtl->args.pretend_args_size == 2);

      p = emit_insn (gen_grow_and_swap ());

      /* Document the stack decrement...  */
      note = F (gen_rtx_SET (stack_pointer_rtx,
			     gen_rtx_MINUS (Pmode, stack_pointer_rtx, GEN_INT (2))));
      add_reg_note (p, REG_FRAME_RELATED_EXPR, note);

      /* ...and the establishment of a new location for the return address.  */
      note = F (gen_rtx_SET (gen_rtx_MEM (Pmode,
					  gen_rtx_PLUS (Pmode,
							stack_pointer_rtx,
							GEN_INT (-2))),
			     pc_rtx));
      add_reg_note (p, REG_CFA_OFFSET, note);
      F (p);
    }

  for (i = 15; i >= 4; i--)
    if (cfun->machine->need_to_save [i])
      {
	int seq, count;
	rtx note;

	for (seq = i - 1; seq >= 4 && cfun->machine->need_to_save[seq]; seq --)
	  ;
	count = i - seq;

	if (msp430x)
	  {
	    /* Note: with TARGET_LARGE we still use PUSHM as PUSHX.A is two bytes bigger.  */
	    p = F (emit_insn (gen_pushm (gen_rtx_REG (Pmode, i),
					 GEN_INT (count))));

	    note = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (count + 1));

	    XVECEXP (note, 0, 0)
	      = F (gen_rtx_SET (stack_pointer_rtx,
				gen_rtx_PLUS (Pmode,
					      stack_pointer_rtx,
					      GEN_INT (count * (TARGET_LARGE ? -4 : -2)))));

	    /* *sp-- = R[i-j] */
	    /* sp+N	R10
	       ...
	       sp	R4  */
	    for (j = 0; j < count; j ++)
	      {
		rtx addr;
		int ofs = (count - j - 1) * (TARGET_LARGE ? 4 : 2);

		if (ofs)
		  addr = gen_rtx_PLUS (Pmode, sp, GEN_INT (ofs));
		else
		  addr = stack_pointer_rtx;

		XVECEXP (note, 0, j + 1) =
		  F (gen_rtx_SET (gen_rtx_MEM (Pmode, addr),
				  gen_rtx_REG (Pmode, i - j)) );
	      }

	    add_reg_note (p, REG_FRAME_RELATED_EXPR, note);
	    i -= count - 1;
	  }
	else
	  F (emit_insn (gen_push (gen_rtx_REG (Pmode, i))));
      }

  if (frame_pointer_needed)
    F (emit_move_insn (gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM), sp));

  fs = cfun->machine->framesize_locals + cfun->machine->framesize_outgoing;

  increment_stack (- fs);

  emit_insn (gen_prologue_end_marker ());
}

void
msp430_expand_epilogue (int is_eh)
{
  int i;
  int fs;
  int helper_n = 0;

  if (is_naked_func ())
    {
      /* We must generate some RTX as thread_prologue_and_epilogue_insns()
	 examines the output of the gen_epilogue() function.  */
      emit_insn (gen_rtx_CLOBBER (VOIDmode, GEN_INT (0)));
      return;
    }

  if (cfun->machine->need_to_save [10])
    {
      /* Check for a helper function.  */
      helper_n = 7; /* For when the loop below never sees a match.  */
      for (i = 9; i >= 4; i--)
	if (!cfun->machine->need_to_save [i])
	  {
	    helper_n = 10 - i;
	    for (; i >= 4; i--)
	      if (cfun->machine->need_to_save [i])
		{
		  helper_n = 0;
		  break;
		}
	    break;
	  }
    }

  emit_insn (gen_epilogue_start_marker ());

  if (cfun->decl && strcmp (IDENTIFIER_POINTER (DECL_NAME (cfun->decl)), "main") == 0)
    emit_insn (gen_msp430_refsym_need_exit ());

  if (is_wakeup_func ())
    /* Clear the SCG1, SCG0, OSCOFF and CPUOFF bits in the saved copy of the
       status register current residing on the stack.  When this function
       executes its RETI instruction the SR will be updated with this saved
       value, thus ensuring that the processor is woken up from any low power
       state in which it may be residing.  */
    emit_insn (gen_bic_SR (GEN_INT (0xf0)));

  fs = cfun->machine->framesize_locals + cfun->machine->framesize_outgoing;

  increment_stack (fs);

  if (is_eh)
    {
      /* We need to add the right "SP" register save just after the
	 regular ones, so that when we pop it off we're in the EH
	 return frame, not this one.  This overwrites our own return
	 address, but we're not going to be returning anyway.  */
      rtx r12 = gen_rtx_REG (Pmode, 12);
      rtx (*addPmode)(rtx, rtx, rtx) = TARGET_LARGE ? gen_addpsi3 : gen_addhi3;

      /* R12 will hold the new SP.  */
      i = cfun->machine->framesize_regs;
      emit_move_insn (r12, stack_pointer_rtx);
      emit_insn (addPmode (r12, r12, EH_RETURN_STACKADJ_RTX));
      emit_insn (addPmode (r12, r12, GEN_INT (i)));
      emit_move_insn (gen_rtx_MEM (Pmode, plus_constant (Pmode, stack_pointer_rtx, i)), r12);
    }

  for (i = 4; i <= 15; i++)
    if (cfun->machine->need_to_save [i])
      {
	int seq, count;

	for (seq = i + 1; seq <= 15 && cfun->machine->need_to_save[seq]; seq ++)
	  ;
	count = seq - i;

	if (msp430x)
	  {
	    /* Note: With TARGET_LARGE we still use
	       POPM as POPX.A is two bytes bigger.  */
	    emit_insn (gen_popm (stack_pointer_rtx, GEN_INT (seq - 1),
				 GEN_INT (count)));
	    i += count - 1;
	  }
	else if (i == 11 - helper_n
		 && ! msp430_is_interrupt_func ()
		 && ! is_reentrant_func ()
		 && ! is_critical_func ()
		 && crtl->args.pretend_args_size == 0
		 /* Calling the helper takes as many bytes as the POP;RET sequence.  */
		 && helper_n > 1
		 && !is_eh)
	  {
	    emit_insn (gen_epilogue_helper (GEN_INT (helper_n)));
	    return;
	  }
	else
	  emit_insn (gen_pop (gen_rtx_REG (Pmode, i)));
      }

  if (is_eh)
    {
      /* Also pop SP, which puts us into the EH return frame.  Except
	 that you can't "pop" sp, you have to just load it off the
	 stack.  */
      emit_move_insn (stack_pointer_rtx, gen_rtx_MEM (Pmode, stack_pointer_rtx));
    }

  if (crtl->args.pretend_args_size)
    emit_insn (gen_swap_and_shrink ());

  if (is_critical_func ())
    emit_insn (gen_pop_intr_state ());
  else if (is_reentrant_func ())
    emit_insn (gen_enable_interrupts ());

  emit_jump_insn (gen_msp_return ());
}

/* Implements EH_RETURN_STACKADJ_RTX.  Saved and used later in
   m32c_emit_eh_epilogue.  */
rtx
msp430_eh_return_stackadj_rtx (void)
{
  if (!cfun->machine->eh_stack_adjust)
    {
      rtx sa;

      sa = gen_rtx_REG (Pmode, 15);
      cfun->machine->eh_stack_adjust = sa;
    }
  return cfun->machine->eh_stack_adjust;
}

/* This function is called before reload, to "fix" the stack in
   preparation for an EH return.  */
void
msp430_expand_eh_return (rtx eh_handler)
{
  /* These are all Pmode */
  rtx ap, sa, ra, tmp;

  ap = arg_pointer_rtx;
  sa = msp430_eh_return_stackadj_rtx ();
  ra = eh_handler;

  tmp = ap;
  tmp = gen_rtx_PLUS (Pmode, ap, sa);
  tmp = plus_constant (Pmode, tmp, TARGET_LARGE ? -4 : -2);
  tmp = gen_rtx_MEM (Pmode, tmp);
  emit_move_insn (tmp, ra);
}

#undef  TARGET_INIT_DWARF_REG_SIZES_EXTRA
#define TARGET_INIT_DWARF_REG_SIZES_EXTRA msp430_init_dwarf_reg_sizes_extra
void
msp430_init_dwarf_reg_sizes_extra (tree address)
{
  int i;
  rtx addr = expand_normal (address);
  rtx mem = gen_rtx_MEM (BLKmode, addr);

  /* This needs to match msp430_unwind_word_mode (above).  */
  if (!msp430x)
    return;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      unsigned int dnum = DWARF_FRAME_REGNUM (i);
      unsigned int rnum = DWARF2_FRAME_REG_OUT (dnum, 1);

      if (rnum < DWARF_FRAME_REGISTERS)
	{
	  HOST_WIDE_INT offset = rnum * GET_MODE_SIZE (QImode);

	  emit_move_insn (adjust_address (mem, QImode, offset),
			  gen_int_mode (4, QImode));
	}
    }
}

/* This is a list of MD patterns that implement fixed-count shifts.  */
static struct
{
  const char *name;
  int count;
  int need_430x;
  rtx (*genfunc)(rtx,rtx);
}
  const_shift_helpers[] =
{
#define CSH(N,C,X,G) { "__mspabi_" N, C, X, gen_##G }

  CSH ("slli", 1, 1, slli_1),
  CSH ("slll", 1, 1, slll_1),
  CSH ("slll", 2, 1, slll_2),

  CSH ("srai", 1, 0, srai_1),
  CSH ("sral", 1, 0, sral_1),
  CSH ("sral", 2, 0, sral_2),

  CSH ("srll", 1, 0, srll_1),
  CSH ("srll", 2, 1, srll_2x),
  { 0, 0, 0, 0 }
#undef CSH
};

/* The MSP430 ABI defines a number of helper functions that should be
   used for, for example, 32-bit shifts.  This function is called to
   emit such a function, using the table above to optimize some
   cases.  */
void
msp430_expand_helper (rtx *operands, const char *helper_name, bool const_variants)
{
  rtx c, f;
  char *helper_const = NULL;
  int arg2 = 13;
  int arg1sz = 1;
  machine_mode arg0mode = GET_MODE (operands[0]);
  machine_mode arg1mode = GET_MODE (operands[1]);
  machine_mode arg2mode = GET_MODE (operands[2]);
  int have_430x = msp430x ? 1 : 0;

  if (CONST_INT_P (operands[2]))
    {
      int i;

      for (i=0; const_shift_helpers[i].name; i++)
	{
	  if (const_shift_helpers[i].need_430x <= have_430x
	      && strcmp (helper_name, const_shift_helpers[i].name) == 0
	      && INTVAL (operands[2]) == const_shift_helpers[i].count)
	    {
	      emit_insn (const_shift_helpers[i].genfunc (operands[0], operands[1]));
	      return;
	    }
	}
    }

  if (arg1mode == VOIDmode)
    arg1mode = arg0mode;
  if (arg2mode == VOIDmode)
    arg2mode = arg0mode;

  if (arg1mode == SImode)
    {
      arg2 = 14;
      arg1sz = 2;
    }

  if (const_variants
      && CONST_INT_P (operands[2])
      && INTVAL (operands[2]) >= 1
      && INTVAL (operands[2]) <= 15)
    {
      /* Note that the INTVAL is limited in value and length by the conditional above.  */
      int len = strlen (helper_name) + 4;
      helper_const = (char *) xmalloc (len);
      snprintf (helper_const, len, "%s_%d", helper_name, (int) INTVAL (operands[2]));
    }

  emit_move_insn (gen_rtx_REG (arg1mode, 12),
		  operands[1]);
  if (!helper_const)
    emit_move_insn (gen_rtx_REG (arg2mode, arg2),
		    operands[2]);

  c = gen_call_value_internal (gen_rtx_REG (arg0mode, 12),
			       gen_rtx_SYMBOL_REF (VOIDmode, helper_const ? helper_const : helper_name),
			       GEN_INT (0));
  c = emit_call_insn (c);
  RTL_CONST_CALL_P (c) = 1;

  f = 0;
  use_regs (&f, 12, arg1sz);
  if (!helper_const)
    use_regs (&f, arg2, 1);
  add_function_usage_to (c, f);

  emit_move_insn (operands[0],
		  gen_rtx_REG (arg0mode, 12));
}

/* Called by cbranch<mode>4 to coerce operands into usable forms.  */
void
msp430_fixup_compare_operands (machine_mode my_mode, rtx * operands)
{
  /* constants we're looking for, not constants which are allowed.  */
  int const_op_idx = 1;

  if (msp430_reversible_cmp_operator (operands[0], VOIDmode))
    const_op_idx = 2;

  if (GET_CODE (operands[const_op_idx]) != REG
      && GET_CODE (operands[const_op_idx]) != MEM)
    operands[const_op_idx] = copy_to_mode_reg (my_mode, operands[const_op_idx]);
}

/* Simplify_gen_subreg() doesn't handle memory references the way we
   need it to below, so we use this function for when we must get a
   valid subreg in a "natural" state.  */
rtx
msp430_subreg (machine_mode mode, rtx r, machine_mode omode, int byte)
{
  rtx rv;

  if (GET_CODE (r) == SUBREG
      && SUBREG_BYTE (r) == 0)
    {
      rtx ireg = SUBREG_REG (r);
      machine_mode imode = GET_MODE (ireg);

      /* special case for (HI (SI (PSI ...), 0)) */
      if (imode == PSImode
	  && mode == HImode
	  && byte == 0)
	rv = gen_rtx_SUBREG (mode, ireg, byte);
      else
	rv = simplify_gen_subreg (mode, ireg, imode, byte);
    }
  else if (GET_CODE (r) == MEM)
    rv = adjust_address (r, mode, byte);
  else if (GET_CODE (r) == SYMBOL_REF
	   && (byte == 0 || byte == 2)
	   && mode == HImode)
    {
      rv = gen_rtx_ZERO_EXTRACT (HImode, r, GEN_INT (16), GEN_INT (8*byte));
      rv = gen_rtx_CONST (HImode, r);
    }
  else
    rv = simplify_gen_subreg (mode, r, omode, byte);

  if (!rv)
    gcc_unreachable ();

  return rv;
}

/* Called by movsi_x to generate the HImode operands.  */
void
msp430_split_movsi (rtx *operands)
{
  rtx op00, op02, op10, op12;

  op00 = msp430_subreg (HImode, operands[0], SImode, 0);
  op02 = msp430_subreg (HImode, operands[0], SImode, 2);

  if (GET_CODE (operands[1]) == CONST
      || GET_CODE (operands[1]) == SYMBOL_REF)
    {
      op10 = gen_rtx_ZERO_EXTRACT (HImode, operands[1], GEN_INT (16), GEN_INT (0));
      op10 = gen_rtx_CONST (HImode, op10);
      op12 = gen_rtx_ZERO_EXTRACT (HImode, operands[1], GEN_INT (16), GEN_INT (16));
      op12 = gen_rtx_CONST (HImode, op12);
    }
  else
    {
      op10 = msp430_subreg (HImode, operands[1], SImode, 0);
      op12 = msp430_subreg (HImode, operands[1], SImode, 2);
    }

  if (rtx_equal_p (operands[0], operands[1]))
    {
      operands[2] = op02;
      operands[4] = op12;
      operands[3] = op00;
      operands[5] = op10;
    }
  else if (rtx_equal_p (op00, op12)
	   /* Catch the case where we are loading (rN, rN+1) from mem (rN).  */
	   || (REG_P (op00) && reg_mentioned_p (op00, op10))
	   /* Or storing (rN) into mem (rN).  */
	   || (REG_P (op10) && reg_mentioned_p (op10, op00))
	   )
    {
      operands[2] = op02;
      operands[4] = op12;
      operands[3] = op00;
      operands[5] = op10;
    }
  else
    {
      operands[2] = op00;
      operands[4] = op10;
      operands[3] = op02;
      operands[5] = op12;
    }
}


/* The MSPABI specifies the names of various helper functions, many of
   which are compatible with GCC's helpers.  This table maps the GCC
   name to the MSPABI name.  */
static const struct
{
  char const * const gcc_name;
  char const * const ti_name;
}
  helper_function_name_mappings [] =
{
  /* Floating point to/from integer conversions.  */
  { "__truncdfsf2", "__mspabi_cvtdf" },
  { "__extendsfdf2", "__mspabi_cvtfd" },
  { "__fixdfhi", "__mspabi_fixdi" },
  { "__fixdfsi", "__mspabi_fixdli" },
  { "__fixdfdi", "__mspabi_fixdlli" },
  { "__fixunsdfhi", "__mspabi_fixdu" },
  { "__fixunsdfsi", "__mspabi_fixdul" },
  { "__fixunsdfdi", "__mspabi_fixdull" },
  { "__fixsfhi", "__mspabi_fixfi" },
  { "__fixsfsi", "__mspabi_fixfli" },
  { "__fixsfdi", "__mspabi_fixflli" },
  { "__fixunsfhi", "__mspabi_fixfu" },
  { "__fixunsfsi", "__mspabi_fixful" },
  { "__fixunsfdi", "__mspabi_fixfull" },
  { "__floathisf", "__mspabi_fltif" },
  { "__floatsisf", "__mspabi_fltlif" },
  { "__floatdisf", "__mspabi_fltllif" },
  { "__floathidf", "__mspabi_fltid" },
  { "__floatsidf", "__mspabi_fltlid" },
  { "__floatdidf", "__mspabi_fltllid" },
  { "__floatunhisf", "__mspabi_fltuf" },
  { "__floatunsisf", "__mspabi_fltulf" },
  { "__floatundisf", "__mspabi_fltullf" },
  { "__floatunhidf", "__mspabi_fltud" },
  { "__floatunsidf", "__mspabi_fltuld" },
  { "__floatundidf", "__mspabi_fltulld" },

  /* Floating point comparisons.  */
  /* GCC uses individual functions for each comparison, TI uses one
     compare <=> function.  */

  /* Floating point arithmatic */
  { "__adddf3", "__mspabi_addd" },
  { "__addsf3", "__mspabi_addf" },
  { "__divdf3", "__mspabi_divd" },
  { "__divsf3", "__mspabi_divf" },
  { "__muldf3", "__mspabi_mpyd" },
  { "__mulsf3", "__mspabi_mpyf" },
  { "__subdf3", "__mspabi_subd" },
  { "__subsf3", "__mspabi_subf" },
  /* GCC does not use helper functions for negation */

  /* Integer multiply, divide, remainder.  */
  { "__mulhi3", "__mspabi_mpyi" },
  { "__mulsi3", "__mspabi_mpyl" },
  { "__muldi3", "__mspabi_mpyll" },
#if 0
  /* Clarify signed vs unsigned first.  */
  { "__mulhisi3", "__mspabi_mpysl" }, /* gcc doesn't use widening multiply (yet?) */
  { "__mulsidi3", "__mspabi_mpysll" }, /* gcc doesn't use widening multiply (yet?) */
#endif

  { "__divhi3", "__mspabi_divi" },
  { "__divsi3", "__mspabi_divli" },
  { "__divdi3", "__mspabi_divlli" },
  { "__udivhi3", "__mspabi_divu" },
  { "__udivsi3", "__mspabi_divul" },
  { "__udivdi3", "__mspabi_divull" },
  { "__modhi3", "__mspabi_remi" },
  { "__modsi3", "__mspabi_remli" },
  { "__moddi3", "__mspabi_remlli" },
  { "__umodhi3", "__mspabi_remu" },
  { "__umodsi3", "__mspabi_remul" },
  { "__umoddi3", "__mspabi_remull" },

  /* Bitwise operations.  */
  /* Rotation - no rotation support yet.  */
  /* Logical left shift - gcc already does these itself.  */
  /* Arithmetic left shift - gcc already does these itself.  */
  /* Arithmetic right shift - gcc already does these itself.  */

  { NULL, NULL }
};

/* Returns true if the current MCU supports an F5xxx series
   hardware multiper.  */

bool
msp430_use_f5_series_hwmult (void)
{
  static const char * cached_match = NULL;
  static bool         cached_result;

  if (msp430_hwmult_type == MSP430_HWMULT_F5SERIES)
    return true;

  if (target_mcu == NULL || msp430_hwmult_type != MSP430_HWMULT_AUTO)
    return false;

  if (target_mcu == cached_match)
    return cached_result;

  cached_match = target_mcu;

  if (strncasecmp (target_mcu, "msp430f5", 8) == 0)
    return cached_result = true;
  if (strncasecmp (target_mcu, "msp430fr5", 9) == 0)
    return cached_result = true;
  if (strncasecmp (target_mcu, "msp430f6", 8) == 0)
    return cached_result = true;

  int i;

  /* FIXME: This array is alpha sorted - we could use a binary search.  */
  for (i = ARRAY_SIZE (msp430_mcu_data); i--;)
    if (strcasecmp (target_mcu, msp430_mcu_data[i].name) == 0)
      return cached_result = msp430_mcu_data[i].hwmpy == 8;

  return cached_result = false;
}

/* Returns true if the current MCU has a second generation
   32-bit hardware multiplier.  */

static bool
use_32bit_hwmult (void)
{
  static const char * cached_match = NULL;
  static bool         cached_result;
  int i;

  if (msp430_hwmult_type == MSP430_HWMULT_LARGE)
    return true;

  if (target_mcu == NULL || msp430_hwmult_type != MSP430_HWMULT_AUTO)
    return false;

  if (target_mcu == cached_match)
    return cached_result;

  cached_match = target_mcu;

  /* FIXME: This array is alpha sorted - we could use a binary search.  */
  for (i = ARRAY_SIZE (msp430_mcu_data); i--;)
    if (strcasecmp (target_mcu, msp430_mcu_data[i].name) == 0)
      return cached_result = msp430_mcu_data[i].hwmpy == 4;

  return cached_result = false;
}

/* Returns true if the current MCU does not have a
   hardware multiplier of any kind.  */

static bool
msp430_no_hwmult (void)
{
  static const char * cached_match = NULL;
  static bool         cached_result;
  int i;

  if (msp430_hwmult_type == MSP430_HWMULT_NONE)
    return true;

  if (msp430_hwmult_type != MSP430_HWMULT_AUTO)
    return false;

  if (target_mcu == NULL)
    return true;

  if (target_mcu == cached_match)
    return cached_result;

  cached_match = target_mcu;

  /* FIXME: This array is alpha sorted - we could use a binary search.  */
  for (i = ARRAY_SIZE (msp430_mcu_data); i--;)
    if (strcasecmp (target_mcu, msp430_mcu_data[i].name) == 0)
      return cached_result = msp430_mcu_data[i].hwmpy == 0;

  /* If we do not recognise the MCU name, we assume that it does not support
     any kind of hardware multiply - this is the safest assumption to make.  */
  return cached_result = true;
}

/* This function does the same as the default, but it will replace GCC
   function names with the MSPABI-specified ones.  */

void
msp430_output_labelref (FILE *file, const char *name)
{
  int i;

  for (i = 0; helper_function_name_mappings [i].gcc_name; i++)
    if (strcmp (helper_function_name_mappings [i].gcc_name, name) == 0)
      {
	name = helper_function_name_mappings [i].ti_name;
	break;
      }

  /* If we have been given a specific MCU name then we may be
     able to make use of its hardware multiply capabilities.  */
  if (msp430_hwmult_type != MSP430_HWMULT_NONE)
    {
      if (strcmp ("__mspabi_mpyi", name) == 0)
	{
	  if (msp430_use_f5_series_hwmult ())
	    name = "__mulhi2_f5";
	  else if (! msp430_no_hwmult ())
	    name = "__mulhi2";
	}
      else if (strcmp ("__mspabi_mpyl", name) == 0)
	{
	  if (msp430_use_f5_series_hwmult ())
	    name = "__mulsi2_f5";
	  else if (use_32bit_hwmult ())
	    name = "__mulsi2_hw32";
	  else if (! msp430_no_hwmult ())
	    name = "__mulsi2";
	}
    }

  fputs (name, file);
}

/* Common code for msp430_print_operand...  */

static void
msp430_print_operand_raw (FILE * file, rtx op)
{
  HOST_WIDE_INT i;

  switch (GET_CODE (op))
    {
    case REG:
      fprintf (file, "%s", reg_names [REGNO (op)]);
      break;

    case CONST_INT:
      i = INTVAL (op);
      if (TARGET_ASM_HEX)
	fprintf (file, "%#" HOST_WIDE_INT_PRINT "x", i);
      else
	fprintf (file, "%" HOST_WIDE_INT_PRINT "d", i);
      break;

    case CONST:
    case PLUS:
    case MINUS:
    case SYMBOL_REF:
    case LABEL_REF:
      output_addr_const (file, op);
      break;

    default:
      print_rtl (file, op);
      break;
    }
}

#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS	msp430_print_operand_addr

/* Output to stdio stream FILE the assembler syntax for an
   instruction operand that is a memory reference whose address
   is ADDR.  */

static void
msp430_print_operand_addr (FILE * file, machine_mode /*mode*/, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case PLUS:
      msp430_print_operand_raw (file, XEXP (addr, 1));
      gcc_assert (REG_P (XEXP (addr, 0)));
      fprintf (file, "(%s)", reg_names [REGNO (XEXP (addr, 0))]);
      return;

    case REG:
      fprintf (file, "@");
      break;

    case CONST:
    case CONST_INT:
    case SYMBOL_REF:
    case LABEL_REF:
      fprintf (file, "&");
      break;

    default:
      break;
    }

  msp430_print_operand_raw (file, addr);
}

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND		msp430_print_operand

/* A   low 16-bits of int/lower of register pair
   B   high 16-bits of int/higher of register pair
   C   bits 32-47 of a 64-bit value/reg 3 of a DImode value
   D   bits 48-63 of a 64-bit value/reg 4 of a DImode value
   H   like %B (for backwards compatibility)
   I   inverse of value
   J   an integer without a # prefix
   L   like %A (for backwards compatibility)
   O   offset of the top of the stack
   Q   like X but generates an A postfix
   R   inverse of condition code, unsigned.
   X   X instruction postfix in large mode
   Y   value - 4
   Z   value - 1
   b   .B or .W or .A, depending upon the mode
   p   bit position
   r   inverse of condition code
   x   like X but only for pointers.  */

static void
msp430_print_operand (FILE * file, rtx op, int letter)
{
  rtx addr;

  /* We can't use c, n, a, or l.  */
  switch (letter)
    {
    case 'Z':
      gcc_assert (CONST_INT_P (op));
      /* Print the constant value, less one.  */
      fprintf (file, "#%ld", INTVAL (op) - 1);
      return;
    case 'Y':
      gcc_assert (CONST_INT_P (op));
      /* Print the constant value, less four.  */
      fprintf (file, "#%ld", INTVAL (op) - 4);
      return;
    case 'I':
      if (GET_CODE (op) == CONST_INT)
	{
	  /* Inverse of constants */
	  int i = INTVAL (op);
	  fprintf (file, "%d", ~i);
	  return;
	}
      op = XEXP (op, 0);
      break;
    case 'r': /* Conditional jump where the condition is reversed.  */
      switch (GET_CODE (op))
	{
	case EQ: fprintf (file, "NE"); break;
	case NE: fprintf (file, "EQ"); break;
	case GEU: fprintf (file, "LO"); break;
	case LTU: fprintf (file, "HS"); break;
	case GE: fprintf (file, "L"); break;
	case LT: fprintf (file, "GE"); break;
	  /* Assume these have reversed operands.  */
	case GTU: fprintf (file, "HS"); break;
	case LEU: fprintf (file, "LO"); break;
	case GT: fprintf (file, "GE"); break;
	case LE: fprintf (file, "L"); break;
	default:
	  msp430_print_operand_raw (file, op);
	  break;
	}
      return;
    case 'R': /* Conditional jump where the operands are reversed.  */
      switch (GET_CODE (op))
	{
	case GTU: fprintf (file, "LO"); break;
	case LEU: fprintf (file, "HS"); break;
	case GT: fprintf (file, "L"); break;
	case LE: fprintf (file, "GE"); break;
	default:
	  msp430_print_operand_raw (file, op);
	  break;
	}
      return;
    case 'p': /* Bit position. 0 == 0x01, 3 = 0x08 etc.  */
      gcc_assert (CONST_INT_P (op));
      fprintf (file, "#%d", 1 << INTVAL (op));
      return;
    case 'b':
      switch (GET_MODE (op))
	{
	case E_QImode: fprintf (file, ".B"); return;
	case E_HImode: fprintf (file, ".W"); return;
	case E_PSImode: fprintf (file, ".A"); return;
	case E_SImode: fprintf (file, ".A"); return;
	default:
	  return;
	}
    case 'A':
    case 'L': /* Low half.  */
      switch (GET_CODE (op))
	{
	case MEM:
	  op = adjust_address (op, Pmode, 0);
	  break;
	case REG:
	  break;
	case CONST_INT:
	  op = GEN_INT (INTVAL (op) & 0xffff);
	  letter = 0;
	  break;
	default:
	  /* If you get here, figure out a test case :-) */
	  gcc_unreachable ();
	}
      break;
    case 'B':
    case 'H': /* high half */
      switch (GET_CODE (op))
	{
	case MEM:
	  op = adjust_address (op, Pmode, 2);
	  break;
	case REG:
	  op = gen_rtx_REG (Pmode, REGNO (op) + 1);
	  break;
	case CONST_INT:
	  op = GEN_INT (INTVAL (op) >> 16);
	  letter = 0;
	  break;
	default:
	  /* If you get here, figure out a test case :-) */
	  gcc_unreachable ();
	}
      break;
    case 'C':
      switch (GET_CODE (op))
	{
	case MEM:
	  op = adjust_address (op, Pmode, 3);
	  break;
	case REG:
	  op = gen_rtx_REG (Pmode, REGNO (op) + 2);
	  break;
	case CONST_INT:
	  op = GEN_INT ((long long) INTVAL (op) >> 32);
	  letter = 0;
	  break;
	default:
	  /* If you get here, figure out a test case :-) */
	  gcc_unreachable ();
	}
      break;
    case 'D':
      switch (GET_CODE (op))
	{
	case MEM:
	  op = adjust_address (op, Pmode, 4);
	  break;
	case REG:
	  op = gen_rtx_REG (Pmode, REGNO (op) + 3);
	  break;
	case CONST_INT:
	  op = GEN_INT ((long long) INTVAL (op) >> 48);
	  letter = 0;
	  break;
	default:
	  /* If you get here, figure out a test case :-) */
	  gcc_unreachable ();
	}
      break;

    case 'X':
      /* This is used to turn, for example, an ADD opcode into an ADDX
	 opcode when we're using 20-bit addresses.  */
      if (TARGET_LARGE || GET_MODE (op) == PSImode)
	fprintf (file, "X");
      /* We don't care which operand we use, but we want 'X' in the MD
	 file, so we do it this way.  */
      return;

    case 'x':
      /* Similarly, but only for PSImodes.  BIC, for example, needs this.  */
      if (GET_MODE (op) == PSImode)
	fprintf (file, "X");
      return;

    case 'Q':
      /* Likewise, for BR -> BRA.  */
      if (TARGET_LARGE)
	fprintf (file, "A");
      return;

    case 'O':
      /* Computes the offset to the top of the stack for the current frame.
	 This has to be done here rather than in, say, msp430_expand_builtin()
	 because builtins are expanded before the frame layout is determined.  */
      fprintf (file, "%d",
	       msp430_initial_elimination_offset (ARG_POINTER_REGNUM, STACK_POINTER_REGNUM)
	       - (TARGET_LARGE ? 4 : 2));
      return;

    case 'J':
      gcc_assert (GET_CODE (op) == CONST_INT);
    case 0:
      break;
    default:
      output_operand_lossage ("invalid operand prefix");
      return;
    }

  switch (GET_CODE (op))
    {
    case REG:
      msp430_print_operand_raw (file, op);
      break;

    case MEM:
      addr = XEXP (op, 0);
      msp430_print_operand_addr (file, GET_MODE (op), addr);
      break;

    case CONST:
      if (GET_CODE (XEXP (op, 0)) == ZERO_EXTRACT)
	{
	  op = XEXP (op, 0);
	  switch (INTVAL (XEXP (op, 2)))
	    {
	    case 0:
	      fprintf (file, "#lo (");
	      msp430_print_operand_raw (file, XEXP (op, 0));
	      fprintf (file, ")");
	      break;
	  
	    case 16:
	      fprintf (file, "#hi (");
	      msp430_print_operand_raw (file, XEXP (op, 0));
	      fprintf (file, ")");
	      break;

	    default:
	      output_operand_lossage ("invalid zero extract");
	      break;
	    }
	  break;
	}
      /* Fall through.  */
    case CONST_INT:
    case SYMBOL_REF:
    case LABEL_REF:
      if (letter == 0)
	fprintf (file, "#");
      msp430_print_operand_raw (file, op);
      break;

    case EQ: fprintf (file, "EQ"); break;
    case NE: fprintf (file, "NE"); break;
    case GEU: fprintf (file, "HS"); break;
    case LTU: fprintf (file, "LO"); break;
    case GE: fprintf (file, "GE"); break;
    case LT: fprintf (file, "L"); break;

    default:
      print_rtl (file, op);
      break;
    }
}


/* Frame stuff.  */

rtx
msp430_return_addr_rtx (int count)
{
  int ra_size;
  if (count)
    return NULL_RTX;

  ra_size = TARGET_LARGE ? 4 : 2;
  if (crtl->args.pretend_args_size)
    ra_size += 2;

  return gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, arg_pointer_rtx, GEN_INT (- ra_size)));
}

rtx
msp430_incoming_return_addr_rtx (void)
{
  return gen_rtx_MEM (Pmode, stack_pointer_rtx);
}

/* Instruction generation stuff.  */

/* Generate a sequence of instructions to sign-extend an HI
   value into an SI value.  Handles the tricky case where
   we are overwriting the destination.  */

const char *
msp430x_extendhisi (rtx * operands)
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    /* Low word of dest == source word.  */
    return "BIT.W\t#0x8000, %L0 { SUBC.W\t%H0, %H0 { INV.W\t%H0, %H0"; /* 8-bytes.  */

  if (! msp430x)
    /* Note: This sequence is approximately the same length as invoking a helper
       function to perform the sign-extension, as in:

         MOV.W  %1, %L0
	 MOV.W  %1, r12
	 CALL   __mspabi_srai_15
	 MOV.W  r12, %H0

       but this version does not involve any function calls or using argument
       registers, so it reduces register pressure.  */
    return "MOV.W\t%1, %L0 { BIT.W\t#0x8000, %L0 { SUBC.W\t%H0, %H0 { INV.W\t%H0, %H0"; /* 10-bytes.  */

  if (REGNO (operands[0]) + 1 == REGNO (operands[1]))
    /* High word of dest == source word.  */
    return "MOV.W\t%1, %L0 { RPT\t#15 { RRAX.W\t%H0"; /* 6-bytes.  */

  /* No overlap between dest and source.  */
  return "MOV.W\t%1, %L0 { MOV.W\t%1, %H0 { RPT\t#15 { RRAX.W\t%H0"; /* 8-bytes.  */
}

/* Likewise for logical right shifts.  */
const char *
msp430x_logical_shift_right (rtx amount)
{
  /* The MSP430X's logical right shift instruction - RRUM - does
     not use an extension word, so we cannot encode a repeat count.
     Try various alternatives to work around this.  If the count
     is in a register we are stuck, hence the assert.  */
  gcc_assert (CONST_INT_P (amount));

  if (INTVAL (amount) <= 0
      || INTVAL (amount) >= 16)
    return "# nop logical shift.";

  if (INTVAL (amount) > 0
      && INTVAL (amount) < 5)
    return "rrum.w\t%2, %0"; /* Two bytes.  */

  if (INTVAL (amount) > 4
      && INTVAL (amount) < 9)
    return "rrum.w\t#4, %0 { rrum.w\t%Y2, %0 "; /* Four bytes.  */

  /* First we logically shift right by one.  Now we know
     that the top bit is zero and we can use the arithmetic
     right shift instruction to perform the rest of the shift.  */
  return "rrum.w\t#1, %0 { rpt\t%Z2 { rrax.w\t%0"; /* Six bytes.  */
}

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-msp430.h"
