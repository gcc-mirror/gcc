/******************************************************************
***                                                             ***
***     crt0 for __BELOW100__ attribute test with SID           ***
***                                                             ***
******************************************************************/

                                /*************************************/
                                /** Interrupt vectors at 0x8000     **/
                                /*************************************/
        .section .int_vec,"ax"
        .global _start
        .align 1
_start:
        jmpf _int_reset
        //jmpf _int_basetimer
        //jmpf _int_timer0
        //jmpf _int_timer1
        //jmpf _int_irq_4
        //jmpf _int_irq_5
        //jmpf _int_port0
        //jmpf _int_port1
        //jmpf _int_irq_8
        //jmpf _int_irq_9
        //jmpf _int_irq_a
        //jmpf _int_irq_b
        //jmpf _int_irq_c
        //jmpf _int_irq_d
        //jmpf _int_irq_e
        //jmpf _int_irq_f
                                /*************************************/
                                /** reset code                      **/
                                /*************************************/
        .text
_int_reset:
                                /*************************************/
                                /** setup stack pointer             **/
                                /*************************************/
        mov sp,#__stack
                                /*************************************/
                                /** zero .bss section               **/
                                /*************************************/
        mov r0,#__bss_start
        mov r1,#__bss_end
        mov r2,#0
1:      mov.w (r0++),r2
        blt r0,r1,1b
                                /*************************************/
                                /** copy inital value for .data     **/
                                /*************************************/
        mov r1,#__data_start
        mov r3,#__data_end
        mov r0,#@lo(__rdata)
        mov r8,#@hi(__rdata)
2:      movf.w r2,(r0++)
        bnz r0,#0,3f
        add r8,#1
3:      mov.w (r1++),r2
        blt r1,r3,2b
                                /*************************************/
                                /** call hardware init routine      **/
                                /*************************************/
        callf _hwinit
                                /*************************************/
                                /** call initializaton routines     **/
                                /*************************************/
        callf _init
                                /*************************************/
                                /** setup fini routines to be       **/
                                /**  called from exit               **/
                                /*************************************/
        mov r2,#@fptr(_fini)
        callf atexit
                                /*************************************/
                                /** call main() with empty          **/
                                /**  argc/argv/envp                 **/
                                /*************************************/
        mov r2,#0
        mov r3,#0
        mov r4,#0
        callf main
                                /*************************************/
                                /** return from main()              **/
                                /*************************************/
        callf exit
                                /*************************************/
                                /** should never reach this code    **/
                                /*************************************/
        jmpf _start
                                /*************************************/
                                /** default h/w initialize routine  **/
                                /** and default _init/_finit for    **/
                                /**  -nostartfiles option           **/
                                /*************************************/
        .globl _hwinit
        .weak _hwinit
_hwinit:
        .globl _init
        .weak _init
_init:
        .globl _fini
        .weak _fini
_fini:
        ret

/******************************************************************
*******************************************************************
***                                                             ***
***       Chip information data for LC59_32K                    ***
***                 Written by T.Matsukawa                      ***
***                                                             ***
*******************************************************************
******************************************************************/

                                /*************************************/
                                /** Define convenient macros        **/
                                /*************************************/
#define BCD(x)  (((x)/10)%10)*0x10+((x)%10)
#define BCD4(x) BCD((x)/100),BCD(x)
#define BCD6(x) BCD((x)/10000),BCD((x)/100),BCD(x)
                                /*************************************/
                                /** Define memory sizes             **/
                                /*************************************/
#define RAM_SIZE 0x7E00
#define ROM_SIZE 0x78000
#define VRAM_SIZE 0x0000
#define VRAM_ROW 0
#define VRAM_COLUMN 0
#define CGROM_SIZE 0x0000
#define PROTECT_SIZE 0x0000

                                /*************************************/
                                /** section ".chip_info"            **/
                                /*************************************/
        .section .chip_info,"a"
        .space  0xb8,0x00
                                /*************************************/
                                /** B8-BB : User option address     **/
                                /*************************************/
        .word   0x00000
        .global __reset_vector
#if 0x00000==0
        .equ    __reset_vector,0x08000
#else
        .equ    __reset_vector,0x00000
#endif
                                /*************************************/
                                /** BC-BF : Flash Protect address   **/
                                /*************************************/
#if PROTECT_SIZE==0
        .word   0x00000000
#else
        .word   0x08000+ROM_SIZE-PROTECT_SIZE
#endif
                                /*************************************/
                                /** C0-CF : Fixed string            **/
                                /*************************************/
        .ascii  "CHIPINFORMATION"
1:      .space  (0xd0-1b),0x00
                                /*************************************/
                                /** D0-DF : Chipname                **/
                                /*************************************/
        .ascii  "LC59_32K"
2:      .space  (0xe0-2b),0x00
                                /*************************************/
                                /** E0-E1 : Format version(BCD4)    **/
                                /*************************************/
        .byte   0x10, 0x00
        .space  6, 0x00
                                /*************************************/
                                /** E8-F5 : Memory sizes            **/
                                /*************************************/
        .byte   BCD4(ROM_SIZE/1024)
        .byte   BCD6(RAM_SIZE)
        .byte   BCD6(VRAM_SIZE)
        .byte   BCD4(VRAM_ROW)
        .byte   BCD4(VRAM_COLUMN)
        .byte   BCD4(CGROM_SIZE/1024)
        .space  3, 0x00
                                /*************************************/
                                /** F9 : Package type               **/
                                /*************************************/
        .byte   0xff
        .space  6, 0x00

                                /*************************************/
                                /** In order to link BIOS in library**/
                                /*************************************/
        .equ    dummy,__bios_entry
