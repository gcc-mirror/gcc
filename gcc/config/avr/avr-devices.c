/* Copyright (C) 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Anatoly Sokolov (aesok@post.ru)

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
#include "tm.h"

/* List of all known AVR MCU architectures.  */

const struct base_arch_s avr_arch_types[] = {
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, NULL,               "avr2" },  /* unknown device specified */
  { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=1",   "avr1" },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=2",   "avr2" },
  { 0, 0, 0, 1, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=25",  "avr25" },
  { 0, 0, 1, 0, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=3",   "avr3" },
  { 0, 0, 1, 0, 1, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=31",  "avr31" },
  { 0, 0, 1, 1, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=35",  "avr35" },
  { 0, 1, 0, 1, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=4",   "avr4" },
  { 0, 1, 1, 1, 0, 0, 0, 0, 0, 0x0060, "__AVR_ARCH__=5",   "avr5" },
  { 0, 1, 1, 1, 1, 1, 0, 0, 0, 0x0060, "__AVR_ARCH__=51",  "avr51" },
  { 0, 1, 1, 1, 1, 1, 1, 0, 0, 0x0060, "__AVR_ARCH__=6",   "avr6" }
};

/* List of all known AVR MCU types - if updated, it has to be kept
   in sync in several places (FIXME: is there a better way?):
    - here;
    - t-avr (MULTILIB_MATCHES);
    - gas/config/tc-avr.c;
    - avr-libc.  */

const struct mcu_type_s avr_mcu_types[] = {
    /* Classic, <= 8K.  */
  { "avr2",                 ARCH_AVR2, NULL,                        0, 0x0060, "s8515" },
  { "at90s2313",            ARCH_AVR2, "__AVR_AT90S2313__",         1, 0x0060, "s2313" },
  { "at90s2323",            ARCH_AVR2, "__AVR_AT90S2323__",         1, 0x0060, "s2323" },
  { "at90s2333",            ARCH_AVR2, "__AVR_AT90S2333__",         1, 0x0060, "s2333" },
  { "at90s2343",            ARCH_AVR2, "__AVR_AT90S2343__",         1, 0x0060, "s2343" },
  { "attiny22",             ARCH_AVR2, "__AVR_ATtiny22__",          1, 0x0060, "tn22" },
  { "attiny26",             ARCH_AVR2, "__AVR_ATtiny26__",          1, 0x0060, "tn26" },
  { "at90s4414",            ARCH_AVR2, "__AVR_AT90S4414__",         0, 0x0060, "s4414" },
  { "at90s4433",            ARCH_AVR2, "__AVR_AT90S4433__",         1, 0x0060, "s4433" },
  { "at90s4434",            ARCH_AVR2, "__AVR_AT90S4434__",         0, 0x0060, "s4434" },
  { "at90s8515",            ARCH_AVR2, "__AVR_AT90S8515__",         0, 0x0060, "s8515" },
  { "at90c8534",            ARCH_AVR2, "__AVR_AT90C8534__",         0, 0x0060, "c8534" },
  { "at90s8535",            ARCH_AVR2, "__AVR_AT90S8535__",         0, 0x0060, "s8535" },
    /* Classic + MOVW, <= 8K.  */
  { "avr25",                ARCH_AVR25, NULL,                       0, 0x0060, "tn85" },
  { "ata6289",              ARCH_AVR25, "__AVR_ATA6289__",          0, 0x0100, "a6289" },
  { "attiny13",             ARCH_AVR25, "__AVR_ATtiny13__",         1, 0x0060, "tn13" },
  { "attiny13a",            ARCH_AVR25, "__AVR_ATtiny13A__",        1, 0x0060, "tn13a" },
  { "attiny2313",           ARCH_AVR25, "__AVR_ATtiny2313__",       1, 0x0060, "tn2313" },
  { "attiny2313a",          ARCH_AVR25, "__AVR_ATtiny2313A__",      1, 0x0060, "tn2313a" },
  { "attiny24",             ARCH_AVR25, "__AVR_ATtiny24__",         1, 0x0060, "tn24" },
  { "attiny24a",            ARCH_AVR25, "__AVR_ATtiny24A__",        1, 0x0060, "tn24a" },
  { "attiny4313",           ARCH_AVR25, "__AVR_ATtiny4313__",       0, 0x0060, "tn4313" },
  { "attiny44",             ARCH_AVR25, "__AVR_ATtiny44__",         0, 0x0060, "tn44" },
  { "attiny44a",            ARCH_AVR25, "__AVR_ATtiny44A__",        0, 0x0060, "tn44a" },
  { "attiny84",             ARCH_AVR25, "__AVR_ATtiny84__",         0, 0x0060, "tn84" },
  { "attiny84a",            ARCH_AVR25, "__AVR_ATtiny84A__",        0, 0x0060, "tn84" },
  { "attiny25",             ARCH_AVR25, "__AVR_ATtiny25__",         1, 0x0060, "tn25" },
  { "attiny45",             ARCH_AVR25, "__AVR_ATtiny45__",         0, 0x0060, "tn45" },
  { "attiny85",             ARCH_AVR25, "__AVR_ATtiny85__",         0, 0x0060, "tn85" },
  { "attiny261",            ARCH_AVR25, "__AVR_ATtiny261__",        1, 0x0060, "tn261" },
  { "attiny261a",           ARCH_AVR25, "__AVR_ATtiny261A__",       1, 0x0060, "tn261a" },
  { "attiny461",            ARCH_AVR25, "__AVR_ATtiny461__",        0, 0x0060, "tn461" },
  { "attiny461a",           ARCH_AVR25, "__AVR_ATtiny461A__",       0, 0x0060, "tn461a" },
  { "attiny861",            ARCH_AVR25, "__AVR_ATtiny861__",        0, 0x0060, "tn861" },
  { "attiny861a",           ARCH_AVR25, "__AVR_ATtiny861A__",       0, 0x0060, "tn861a" },
  { "attiny43u",            ARCH_AVR25, "__AVR_ATtiny43U__",        0, 0x0060, "tn43u" },
  { "attiny87",             ARCH_AVR25, "__AVR_ATtiny87__",         0, 0x0100, "tn87" },
  { "attiny48",             ARCH_AVR25, "__AVR_ATtiny48__",         0, 0x0100, "tn48" },
  { "attiny88",             ARCH_AVR25, "__AVR_ATtiny88__",         0, 0x0100, "tn88" },
  { "at86rf401",            ARCH_AVR25, "__AVR_AT86RF401__",        0, 0x0060, "86401" },
    /* Classic, > 8K, <= 64K.  */
  { "avr3",                 ARCH_AVR3, NULL,                        0, 0x0060, "43355" },
  { "at43usb355",           ARCH_AVR3, "__AVR_AT43USB355__",        0, 0x0060, "43355" },
  { "at76c711",             ARCH_AVR3, "__AVR_AT76C711__",          0, 0x0060, "76711" },
    /* Classic, == 128K.  */
  { "avr31",                ARCH_AVR31, NULL,                       0, 0x0060, "m103" },
  { "atmega103",            ARCH_AVR31, "__AVR_ATmega103__",        0, 0x0060, "m103" },
  { "at43usb320",           ARCH_AVR31, "__AVR_AT43USB320__",       0, 0x0060, "43320" },
    /* Classic + MOVW + JMP/CALL.  */
  { "avr35",                ARCH_AVR35, NULL,                       0, 0x0100, "usb162" },
  { "at90usb82",            ARCH_AVR35, "__AVR_AT90USB82__",        0, 0x0100, "usb82" },
  { "at90usb162",           ARCH_AVR35, "__AVR_AT90USB162__",       0, 0x0100, "usb162" },
  { "atmega8u2",            ARCH_AVR35, "__AVR_ATmega8U2__",        0, 0x0100, "m8u2" },
  { "atmega16u2",           ARCH_AVR35, "__AVR_ATmega16U2__",       0, 0x0100, "m16u2" },
  { "atmega32u2",           ARCH_AVR35, "__AVR_ATmega32U2__",       0, 0x0100, "m32u2" },
  { "attiny167",            ARCH_AVR35, "__AVR_ATtiny167__",        0, 0x0100, "tn167" },
    /* Enhanced, <= 8K.  */
  { "avr4",                 ARCH_AVR4, NULL,                        0, 0x0060, "m8" },
  { "atmega8",              ARCH_AVR4, "__AVR_ATmega8__",           0, 0x0060, "m8" },
  { "atmega48",             ARCH_AVR4, "__AVR_ATmega48__",          0, 0x0100, "m48" },
  { "atmega48a",            ARCH_AVR4, "__AVR_ATmega48A__",         0, 0x0100, "m48a" },
  { "atmega48p",            ARCH_AVR4, "__AVR_ATmega48P__",         0, 0x0100, "m48p" },
  { "atmega88",             ARCH_AVR4, "__AVR_ATmega88__",          0, 0x0100, "m88" },
  { "atmega88a",            ARCH_AVR4, "__AVR_ATmega88A__",         0, 0x0100, "m88a" },
  { "atmega88p",            ARCH_AVR4, "__AVR_ATmega88P__",         0, 0x0100, "m88p" },
  { "atmega88pa",           ARCH_AVR4, "__AVR_ATmega88PA__",        0, 0x0100, "m88pa" },
  { "atmega8515",           ARCH_AVR4, "__AVR_ATmega8515__",        0, 0x0060, "m8515" },
  { "atmega8535",           ARCH_AVR4, "__AVR_ATmega8535__",        0, 0x0060, "m8535" },
  { "atmega8hva",           ARCH_AVR4, "__AVR_ATmega8HVA__",        0, 0x0100, "m8hva" },
  { "at90pwm1",             ARCH_AVR4, "__AVR_AT90PWM1__",          0, 0x0100, "90pwm1" },
  { "at90pwm2",             ARCH_AVR4, "__AVR_AT90PWM2__",          0, 0x0100, "90pwm2" },
  { "at90pwm2b",            ARCH_AVR4, "__AVR_AT90PWM2B__",         0, 0x0100, "90pwm2b" },
  { "at90pwm3",             ARCH_AVR4, "__AVR_AT90PWM3__",          0, 0x0100, "90pwm3" },
  { "at90pwm3b",            ARCH_AVR4, "__AVR_AT90PWM3B__",         0, 0x0100, "90pwm3b" },
  { "at90pwm81",            ARCH_AVR4, "__AVR_AT90PWM81__",         0, 0x0100, "90pwm81" },
    /* Enhanced, > 8K, <= 64K.  */
  { "avr5",                 ARCH_AVR5, NULL,                        0, 0x0060, "m16" },
  { "atmega16",             ARCH_AVR5, "__AVR_ATmega16__",          0, 0x0060, "m16" },
  { "atmega16a",            ARCH_AVR5, "__AVR_ATmega16A__",         0, 0x0060, "m16a" },
  { "atmega161",            ARCH_AVR5, "__AVR_ATmega161__",         0, 0x0060, "m161" },
  { "atmega162",            ARCH_AVR5, "__AVR_ATmega162__",         0, 0x0100, "m162" },
  { "atmega163",            ARCH_AVR5, "__AVR_ATmega163__",         0, 0x0060, "m163" },
  { "atmega164a",           ARCH_AVR5, "__AVR_ATmega164A__",        0, 0x0100, "m164a" },
  { "atmega164p",           ARCH_AVR5, "__AVR_ATmega164P__",        0, 0x0100, "m164p" },
  { "atmega165",            ARCH_AVR5, "__AVR_ATmega165__",         0, 0x0100, "m165" },
  { "atmega165a",           ARCH_AVR5, "__AVR_ATmega165A__",        0, 0x0100, "m165a" },
  { "atmega165p",           ARCH_AVR5, "__AVR_ATmega165P__",        0, 0x0100, "m165p" },
  { "atmega168",            ARCH_AVR5, "__AVR_ATmega168__",         0, 0x0100, "m168" },
  { "atmega168a",           ARCH_AVR5, "__AVR_ATmega168A__",        0, 0x0100, "m168a" },
  { "atmega168p",           ARCH_AVR5, "__AVR_ATmega168P__",        0, 0x0100, "m168p" },
  { "atmega169",            ARCH_AVR5, "__AVR_ATmega169__",         0, 0x0100, "m169" },
  { "atmega169a",           ARCH_AVR5, "__AVR_ATmega169A__",        0, 0x0100, "m169a" },
  { "atmega169p",           ARCH_AVR5, "__AVR_ATmega169P__",        0, 0x0100, "m169p" },
  { "atmega169pa",          ARCH_AVR5, "__AVR_ATmega169PA__",       0, 0x0100, "m169pa" },
  { "atmega32",             ARCH_AVR5, "__AVR_ATmega32__",          0, 0x0060, "m32" },
  { "atmega323",            ARCH_AVR5, "__AVR_ATmega323__",         0, 0x0060, "m323" },
  { "atmega324a",           ARCH_AVR5, "__AVR_ATmega324A__",        0, 0x0100, "m324a" },
  { "atmega324p",           ARCH_AVR5, "__AVR_ATmega324P__",        0, 0x0100, "m324p" },
  { "atmega324pa",          ARCH_AVR5, "__AVR_ATmega324PA__",       0, 0x0100, "m324pa" },
  { "atmega325",            ARCH_AVR5, "__AVR_ATmega325__",         0, 0x0100, "m325" },
  { "atmega325a",           ARCH_AVR5, "__AVR_ATmega325A__",        0, 0x0100, "m325a" },
  { "atmega325p",           ARCH_AVR5, "__AVR_ATmega325P__",        0, 0x0100, "m325p" },
  { "atmega3250",           ARCH_AVR5, "__AVR_ATmega3250__",        0, 0x0100, "m3250" },
  { "atmega3250a",          ARCH_AVR5, "__AVR_ATmega3250A__",       0, 0x0100, "m3250a" },
  { "atmega3250p",          ARCH_AVR5, "__AVR_ATmega3250P__",       0, 0x0100, "m3250p" },
  { "atmega328",            ARCH_AVR5, "__AVR_ATmega328__",         0, 0x0100, "m328" },
  { "atmega328p",           ARCH_AVR5, "__AVR_ATmega328P__",        0, 0x0100, "m328p" },
  { "atmega329",            ARCH_AVR5, "__AVR_ATmega329__",         0, 0x0100, "m329" },
  { "atmega329a",           ARCH_AVR5, "__AVR_ATmega329A__",        0, 0x0100, "m329a" },
  { "atmega329p",           ARCH_AVR5, "__AVR_ATmega329P__",        0, 0x0100, "m329p" },
  { "atmega329pa",          ARCH_AVR5, "__AVR_ATmega329PA__",       0, 0x0100, "m329pa" },
  { "atmega3290",           ARCH_AVR5, "__AVR_ATmega3290__",        0, 0x0100, "m3290" },
  { "atmega3290a",          ARCH_AVR5, "__AVR_ATmega3290A__",       0, 0x0100, "m3290a" },
  { "atmega3290p",          ARCH_AVR5, "__AVR_ATmega3290P__",       0, 0x0100, "m3290p" },
  { "atmega406",            ARCH_AVR5, "__AVR_ATmega406__",         0, 0x0100, "m406" },
  { "atmega64",             ARCH_AVR5, "__AVR_ATmega64__",          0, 0x0100, "m64" },
  { "atmega640",            ARCH_AVR5, "__AVR_ATmega640__",         0, 0x0200, "m640" },
  { "atmega644",            ARCH_AVR5, "__AVR_ATmega644__",         0, 0x0100, "m644" },
  { "atmega644a",           ARCH_AVR5, "__AVR_ATmega644A__",        0, 0x0100, "m644a" },
  { "atmega644p",           ARCH_AVR5, "__AVR_ATmega644P__",        0, 0x0100, "m644p" },
  { "atmega644pa",          ARCH_AVR5, "__AVR_ATmega644PA__",       0, 0x0100, "m644pa" },
  { "atmega645",            ARCH_AVR5, "__AVR_ATmega645__",         0, 0x0100, "m645" },
  { "atmega645a",           ARCH_AVR5, "__AVR_ATmega645A__",        0, 0x0100, "m645a" },
  { "atmega645p",           ARCH_AVR5, "__AVR_ATmega645P__",        0, 0x0100, "m645p" },
  { "atmega6450",           ARCH_AVR5, "__AVR_ATmega6450__",        0, 0x0100, "m6450" },
  { "atmega6450a",          ARCH_AVR5, "__AVR_ATmega6450A__",       0, 0x0100, "m6450a" },
  { "atmega6450p",          ARCH_AVR5, "__AVR_ATmega6450P__",       0, 0x0100, "m6450p" },
  { "atmega649",            ARCH_AVR5, "__AVR_ATmega649__",         0, 0x0100, "m649" },
  { "atmega649a",           ARCH_AVR5, "__AVR_ATmega649A__",        0, 0x0100, "m649a" },
  { "atmega649p",           ARCH_AVR5, "__AVR_ATmega649P__",        0, 0x0100, "m649p" },
  { "atmega6490",           ARCH_AVR5, "__AVR_ATmega6490__",        0, 0x0100, "m6490" },
  { "atmega16hva",          ARCH_AVR5, "__AVR_ATmega16HVA__",       0, 0x0100, "m16hva" },
  { "atmega16hva2",         ARCH_AVR5, "__AVR_ATmega16HVA2__",      0, 0x0100, "m16hva2" },
  { "atmega16hvb",          ARCH_AVR5, "__AVR_ATmega16HVB__",       0, 0x0100, "m16hvb" },
  { "atmega32hvb",          ARCH_AVR5, "__AVR_ATmega32HVB__",       0, 0x0100, "m32hvb" },
  { "atmega64hve",          ARCH_AVR5, "__AVR_ATmega64HVE__",       0, 0x0100, "m64hve" },
  { "at90can32",            ARCH_AVR5, "__AVR_AT90CAN32__",         0, 0x0100, "can32" },
  { "at90can64",            ARCH_AVR5, "__AVR_AT90CAN64__",         0, 0x0100, "can64" },
  { "at90pwm216",           ARCH_AVR5, "__AVR_AT90PWM216__",        0, 0x0100, "90pwm216" },
  { "at90pwm316",           ARCH_AVR5, "__AVR_AT90PWM316__",        0, 0x0100, "90pwm316" },
  { "atmega32c1",           ARCH_AVR5, "__AVR_ATmega32C1__",        0, 0x0100, "m32c1" },
  { "atmega64c1",           ARCH_AVR5, "__AVR_ATmega64C1__",        0, 0x0100, "m64c1" },
  { "atmega16m1",           ARCH_AVR5, "__AVR_ATmega16M1__",        0, 0x0100, "m16m1" },
  { "atmega32m1",           ARCH_AVR5, "__AVR_ATmega32M1__",        0, 0x0100, "m32m1" },
  { "atmega64m1",           ARCH_AVR5, "__AVR_ATmega64M1__",        0, 0x0100, "m64m1" },
  { "atmega16u4",           ARCH_AVR5, "__AVR_ATmega16U4__",        0, 0x0100, "m16u4" },
  { "atmega32u4",           ARCH_AVR5, "__AVR_ATmega32U4__",        0, 0x0100, "m32u4" },
  { "atmega32u6",           ARCH_AVR5, "__AVR_ATmega32U6__",        0, 0x0100, "m32u6" },
  { "at90scr100",           ARCH_AVR5, "__AVR_AT90SCR100__",        0, 0x0100, "90scr100" },
  { "at90usb646",           ARCH_AVR5, "__AVR_AT90USB646__",        0, 0x0100, "usb646" },
  { "at90usb647",           ARCH_AVR5, "__AVR_AT90USB647__",        0, 0x0100, "usb647" },
  { "at94k",                ARCH_AVR5, "__AVR_AT94K__",             0, 0x0060, "at94k" },
  { "m3000",                ARCH_AVR5, "__AVR_M3000__",             0, 0x1000, "m3000" },
    /* Enhanced, == 128K.  */
  { "avr51",                ARCH_AVR51, NULL,                       0, 0x0100, "m128" },
  { "atmega128",            ARCH_AVR51, "__AVR_ATmega128__",        0, 0x0100, "m128" },
  { "atmega1280",           ARCH_AVR51, "__AVR_ATmega1280__",       0, 0x0200, "m1280" },
  { "atmega1281",           ARCH_AVR51, "__AVR_ATmega1281__",       0, 0x0200, "m1281" },
  { "atmega1284p",          ARCH_AVR51, "__AVR_ATmega1284P__",      0, 0x0100, "m1284p" },
  { "atmega128rfa1",        ARCH_AVR51, "__AVR_ATmega128RFA1__",    0, 0x0200, "m128rfa1" },
  { "at90can128",           ARCH_AVR51, "__AVR_AT90CAN128__",       0, 0x0100, "can128" },
  { "at90usb1286",          ARCH_AVR51, "__AVR_AT90USB1286__",      0, 0x0100, "usb1286" },
  { "at90usb1287",          ARCH_AVR51, "__AVR_AT90USB1287__",      0, 0x0100, "usb1287" },
    /* 3-Byte PC.  */
  { "avr6",                 ARCH_AVR6, NULL,                        0, 0x0200, "m2561" },
  { "atmega2560",           ARCH_AVR6, "__AVR_ATmega2560__",        0, 0x0200, "m2560" },
  { "atmega2561",           ARCH_AVR6, "__AVR_ATmega2561__",        0, 0x0200, "m2561" },
    /* Assembler only.  */
  { "avr1",                 ARCH_AVR1, NULL,                        0, 0x0060, "s1200" },
  { "at90s1200",            ARCH_AVR1, "__AVR_AT90S1200__",         0, 0x0060, "s1200" },
  { "attiny11",             ARCH_AVR1, "__AVR_ATtiny11__",          0, 0x0060, "tn11" },
  { "attiny12",             ARCH_AVR1, "__AVR_ATtiny12__",          0, 0x0060, "tn12" },
  { "attiny15",             ARCH_AVR1, "__AVR_ATtiny15__",          0, 0x0060, "tn15" },
  { "attiny28",             ARCH_AVR1, "__AVR_ATtiny28__",          0, 0x0060, "tn28" },
    /* End of list.  */
  { NULL,                   ARCH_UNKNOWN, NULL,                     0,      0, NULL }
};

