/*  PR 61692  */
/* { dg-do compile } */

/* Check for ice when exceededing the max #
   of parameters to inline asm. */

int Labels()
{
    label01: label02: label03: label04: label05:
    label06: label07: label08: label09: label10:
    label11: label12: label13: label14: label15:
    label16: label17: label18: label19: label20:
    label21: label22: label23: label24: label25:
    label26: label27: label28: label29: label30:
    label31:

    __asm__ goto ("" /* Works. */
        : /* no outputs */ 
        : /* no inputs */ 
        : /* no clobbers */
        : label01, label02, label03, label04, label05, 
          label06, label07, label08, label09, label10, 
          label11, label12, label13, label14, label15, 
          label16, label17, label18, label19, label20, 
          label21, label22, label23, label24, label25, 
          label26, label27, label28, label29, label30);

    __asm__ goto ("" /* { dg-error "more than 30 operands" } */
        : /* no outputs */ 
        : /* no inputs */ 
        : /* no clobbers */
        : label01, label02, label03, label04, label05, 
          label06, label07, label08, label09, label10, 
          label11, label12, label13, label14, label15, 
          label16, label17, label18, label19, label20, 
          label21, label22, label23, label24, label25, 
          label26, label27, label28, label29, label30, 
          label31);

    return 0;
}

int Labels_and_Inputs()
{
    int b01, b02, b03, b04, b05, b06, b07, b08, b09, b10;
    int b11, b12, b13, b14, b15, b16, b17, b18, b19, b20;
    int b21, b22, b23, b24, b25, b26, b27, b28, b29, b30;
    int b31;

    label01: label02: label03: label04: label05:
    label06: label07: label08: label09: label10:
    label11: label12: label13: label14: label15:
    label16: label17: label18: label19: label20:
    label21: label22: label23: label24: label25:
    label26: label27: label28: label29: label30:
    label31:

    b01 = b02 = b03 = b04 = b05 = b06 = b07 = b08 = b09 = b10 = 0;
    b11 = b12 = b13 = b14 = b15 = b16 = b17 = b18 = b19 = b20 = 0;
    b21 = b22 = b23 = b24 = b25 = b26 = b27 = b28 = b29 = b30 = 0;
    b31 = 0;

    __asm__ goto ("" /* Works. */
      : /* no outputs */
      : "m" (b01), "m" (b02), "m" (b03), "m" (b04), "m" (b05), 
        "m" (b06), "m" (b07), "m" (b08), "m" (b09), "m" (b10), 
        "m" (b11), "m" (b12), "m" (b13), "m" (b14), "m" (b15),
        "m" (b16), "m" (b17), "m" (b18), "m" (b19), "m" (b20), 
        "m" (b21), "m" (b22), "m" (b23), "m" (b24), "m" (b25),
        "m" (b26), "m" (b27), "m" (b28), "m" (b29)
      : /* no clobbers */
      : label01);

    __asm__ goto ("" /* { dg-error "more than 30 operands" } */
      : /* no outputs */
      : "m" (b01), "m" (b02), "m" (b03), "m" (b04), "m" (b05), 
        "m" (b06), "m" (b07), "m" (b08), "m" (b09), "m" (b10), 
        "m" (b11), "m" (b12), "m" (b13), "m" (b14), "m" (b15),
        "m" (b16), "m" (b17), "m" (b18), "m" (b19), "m" (b20), 
        "m" (b21), "m" (b22), "m" (b23), "m" (b24), "m" (b25),
        "m" (b26), "m" (b27), "m" (b28), "m" (b29), "m" (b30)
      : /* no clobbers */
      : label01);

      return 0;
}

int Outputs()
{
    int b01, b02, b03, b04, b05, b06, b07, b08, b09, b10;
    int b11, b12, b13, b14, b15, b16, b17, b18, b19, b20;
    int b21, b22, b23, b24, b25, b26, b27, b28, b29, b30;
    int b31;

    /* Outputs. */
    __asm__ volatile ("" /* Works. */
         : "=m" (b01),  "=m" (b02),  "=m" (b03),  "=m" (b04), "=m" (b05),
           "=m" (b06),  "=m" (b07),  "=m" (b08),  "=m" (b09), "=m" (b10),
           "=m" (b11),  "=m" (b12),  "=m" (b13),  "=m" (b14), "=m" (b15),
           "=m" (b16),  "=m" (b17),  "=m" (b18),  "=m" (b19), "=m" (b20), 
           "=m" (b21),  "=m" (b22),  "=m" (b23),  "=m" (b24), "=m" (b25),
           "=m" (b26),  "=m" (b27),  "=m" (b28),  "=m" (b29), "=m" (b30));

    __asm__ volatile ("" /* { dg-error "more than 30 operands" } */
         : "=m" (b01),  "=m" (b02),  "=m" (b03),  "=m" (b04), "=m" (b05),
           "=m" (b06),  "=m" (b07),  "=m" (b08),  "=m" (b09), "=m" (b10),
           "=m" (b11),  "=m" (b12),  "=m" (b13),  "=m" (b14), "=m" (b15),
           "=m" (b16),  "=m" (b17),  "=m" (b18),  "=m" (b19), "=m" (b20), 
           "=m" (b21),  "=m" (b22),  "=m" (b23),  "=m" (b24), "=m" (b25),
           "=m" (b26),  "=m" (b27),  "=m" (b28),  "=m" (b29), "=m" (b30),
           "=m" (b31));

    return 0;
}

int Inputs()
{
    int b01, b02, b03, b04, b05, b06, b07, b08, b09, b10;
    int b11, b12, b13, b14, b15, b16, b17, b18, b19, b20;
    int b21, b22, b23, b24, b25, b26, b27, b28, b29, b30;
    int b31;

    b01 = b02 = b03 = b04 = b05 = b06 = b07 = b08 = b09 = b10 = 0;
    b11 = b12 = b13 = b14 = b15 = b16 = b17 = b18 = b19 = b20 = 0;
    b21 = b22 = b23 = b24 = b25 = b26 = b27 = b28 = b29 = b30 = 0;
    b31 = 0;

    __asm__ volatile ("" /* Works. */
      : /* no outputs */
      : "m" (b01), "m" (b02), "m" (b03), "m" (b04), "m" (b05), 
        "m" (b06), "m" (b07), "m" (b08), "m" (b09), "m" (b10), 
        "m" (b11), "m" (b12), "m" (b13), "m" (b14), "m" (b15),
        "m" (b16), "m" (b17), "m" (b18), "m" (b19), "m" (b20), 
        "m" (b21), "m" (b22), "m" (b23), "m" (b24), "m" (b25),
        "m" (b26), "m" (b27), "m" (b28), "m" (b29), "m" (b30));

    __asm__ volatile ("" /* { dg-error "more than 30 operands" } */
      : /* no outputs */
      : "m" (b01), "m" (b02), "m" (b03), "m" (b04), "m" (b05), 
        "m" (b06), "m" (b07), "m" (b08), "m" (b09), "m" (b10), 
        "m" (b11), "m" (b12), "m" (b13), "m" (b14), "m" (b15),
        "m" (b16), "m" (b17), "m" (b18), "m" (b19), "m" (b20), 
        "m" (b21), "m" (b22), "m" (b23), "m" (b24), "m" (b25),
        "m" (b26), "m" (b27), "m" (b28), "m" (b29), "m" (b30),
        "m" (b31));

    return 0;
}

int Input_Output()
{
    int b01, b02, b03, b04, b05, b06, b07, b08, b09, b10;
    int b11, b12, b13, b14, b15, b16, b17, b18, b19, b20;
    int b21, b22, b23, b24, b25, b26, b27, b28, b29, b30;
    int b31;

    b01 = b02 = b03 = b04 = b05 = b06 = b07 = b08 = b09 = b10 = 0;
    b11 = b12 = b13 = b14 = b15 = b16 = b17 = b18 = b19 = b20 = 0;
    b21 = b22 = b23 = b24 = b25 = b26 = b27 = b28 = b29 = b30 = 0;
    b31 = 0;

    __asm__ volatile ("" /* Works. */
         : "+m" (b01),  "+m" (b02),  "+m" (b03),  "+m" (b04), "+m" (b05),
           "+m" (b06),  "+m" (b07),  "+m" (b08),  "+m" (b09), "+m" (b10),
           "+m" (b11),  "+m" (b12),  "+m" (b13),  "+m" (b14), "+m" (b15));

    __asm__ volatile ("" /* { dg-error "more than 30 operands" } */
         : "+m" (b01),  "+m" (b02),  "+m" (b03),  "+m" (b04), "+m" (b05),
           "+m" (b06),  "+m" (b07),  "+m" (b08),  "+m" (b09), "+m" (b10),
           "+m" (b11),  "+m" (b12),  "+m" (b13),  "+m" (b14), "+m" (b15),
           "+m" (b16));

    return 0;
}
