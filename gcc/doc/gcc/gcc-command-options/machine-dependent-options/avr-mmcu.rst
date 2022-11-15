..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

  This file is generated automatically using
  gcc/config/avr/gen-avr-mmcu-texi.cc from:
    gcc/config/avr/avr-arch.h
    gcc/config/avr/avr-devices.cc
    gcc/config/avr/avr-mcus.def

  Please do not edit manually.

``avr2``
  "Classic" devices with up to 8 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``attiny22``, ``attiny26``, ``at90s2313``, ``at90s2323``, ``at90s2333``, ``at90s2343``, ``at90s4414``, ``at90s4433``, ``at90s4434``, ``at90c8534``, ``at90s8515``, ``at90s8535``.

``avr25``
  "Classic" devices with up to 8 |nbsp| KiB of program memory and with the ``MOVW`` instruction.

  :samp:`{mcu}=` ``attiny13``, ``attiny13a``, ``attiny24``, ``attiny24a``, ``attiny25``, ``attiny261``, ``attiny261a``, ``attiny2313``, ``attiny2313a``, ``attiny43u``, ``attiny44``, ``attiny44a``, ``attiny45``, ``attiny48``, ``attiny441``, ``attiny461``, ``attiny461a``, ``attiny4313``, ``attiny84``, ``attiny84a``, ``attiny85``, ``attiny87``, ``attiny88``, ``attiny828``, ``attiny841``, ``attiny861``, ``attiny861a``, ``ata5272``, ``ata6616c``, ``at86rf401``.

``avr3``
  "Classic" devices with 16 |nbsp| KiB up to 64 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``at76c711``, ``at43usb355``.

``avr31``
  "Classic" devices with 128 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atmega103``, ``at43usb320``.

``avr35``
  "Classic" devices with 16 |nbsp| KiB up to 64 |nbsp| KiB of program memory and with the ``MOVW`` instruction.

  :samp:`{mcu}=` ``attiny167``, ``attiny1634``, ``atmega8u2``, ``atmega16u2``, ``atmega32u2``, ``ata5505``, ``ata6617c``, ``ata664251``, ``at90usb82``, ``at90usb162``.

``avr4``
  "Enhanced" devices with up to 8 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atmega48``, ``atmega48a``, ``atmega48p``, ``atmega48pa``, ``atmega48pb``, ``atmega8``, ``atmega8a``, ``atmega8hva``, ``atmega88``, ``atmega88a``, ``atmega88p``, ``atmega88pa``, ``atmega88pb``, ``atmega8515``, ``atmega8535``, ``ata6285``, ``ata6286``, ``ata6289``, ``ata6612c``, ``at90pwm1``, ``at90pwm2``, ``at90pwm2b``, ``at90pwm3``, ``at90pwm3b``, ``at90pwm81``.

``avr5``
  "Enhanced" devices with 16 |nbsp| KiB up to 64 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atmega16``, ``atmega16a``, ``atmega16hva``, ``atmega16hva2``, ``atmega16hvb``, ``atmega16hvbrevb``, ``atmega16m1``, ``atmega16u4``, ``atmega161``, ``atmega162``, ``atmega163``, ``atmega164a``, ``atmega164p``, ``atmega164pa``, ``atmega165``, ``atmega165a``, ``atmega165p``, ``atmega165pa``, ``atmega168``, ``atmega168a``, ``atmega168p``, ``atmega168pa``, ``atmega168pb``, ``atmega169``, ``atmega169a``, ``atmega169p``, ``atmega169pa``, ``atmega32``, ``atmega32a``, ``atmega32c1``, ``atmega32hvb``, ``atmega32hvbrevb``, ``atmega32m1``, ``atmega32u4``, ``atmega32u6``, ``atmega323``, ``atmega324a``, ``atmega324p``, ``atmega324pa``, ``atmega324pb``, ``atmega325``, ``atmega325a``, ``atmega325p``, ``atmega325pa``, ``atmega328``, ``atmega328p``, ``atmega328pb``, ``atmega329``, ``atmega329a``, ``atmega329p``, ``atmega329pa``, ``atmega3250``, ``atmega3250a``, ``atmega3250p``, ``atmega3250pa``, ``atmega3290``, ``atmega3290a``, ``atmega3290p``, ``atmega3290pa``, ``atmega406``, ``atmega64``, ``atmega64a``, ``atmega64c1``, ``atmega64hve``, ``atmega64hve2``, ``atmega64m1``, ``atmega64rfr2``, ``atmega640``, ``atmega644``, ``atmega644a``, ``atmega644p``, ``atmega644pa``, ``atmega644rfr2``, ``atmega645``, ``atmega645a``, ``atmega645p``, ``atmega649``, ``atmega649a``, ``atmega649p``, ``atmega6450``, ``atmega6450a``, ``atmega6450p``, ``atmega6490``, ``atmega6490a``, ``atmega6490p``, ``ata5795``, ``ata5790``, ``ata5790n``, ``ata5791``, ``ata6613c``, ``ata6614q``, ``ata5782``, ``ata5831``, ``ata8210``, ``ata8510``, ``ata5702m322``, ``at90pwm161``, ``at90pwm216``, ``at90pwm316``, ``at90can32``, ``at90can64``, ``at90scr100``, ``at90usb646``, ``at90usb647``, ``at94k``, ``m3000``.

``avr51``
  "Enhanced" devices with 128 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atmega128``, ``atmega128a``, ``atmega128rfa1``, ``atmega128rfr2``, ``atmega1280``, ``atmega1281``, ``atmega1284``, ``atmega1284p``, ``atmega1284rfr2``, ``at90can128``, ``at90usb1286``, ``at90usb1287``.

``avr6``
  "Enhanced" devices with 3-byte PC, i.e.: with more than 128 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atmega256rfr2``, ``atmega2560``, ``atmega2561``, ``atmega2564rfr2``.

``avrxmega2``
  "XMEGA" devices with more than 8 |nbsp| KiB and up to 64 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atxmega8e5``, ``atxmega16a4``, ``atxmega16a4u``, ``atxmega16c4``, ``atxmega16d4``, ``atxmega16e5``, ``atxmega32a4``, ``atxmega32a4u``, ``atxmega32c3``, ``atxmega32c4``, ``atxmega32d3``, ``atxmega32d4``, ``atxmega32e5``, ``avr64da28``, ``avr64da32``, ``avr64da48``, ``avr64da64``, ``avr64db28``, ``avr64db32``, ``avr64db48``, ``avr64db64``.

``avrxmega3``
  "XMEGA" devices with up to 64 |nbsp| KiB of combined program memory and RAM, and with program memory visible in the RAM address space.

  :samp:`{mcu}=` ``attiny202``, ``attiny204``, ``attiny212``, ``attiny214``, ``attiny402``, ``attiny404``, ``attiny406``, ``attiny412``, ``attiny414``, ``attiny416``, ``attiny417``, ``attiny804``, ``attiny806``, ``attiny807``, ``attiny814``, ``attiny816``, ``attiny817``, ``attiny1604``, ``attiny1606``, ``attiny1607``, ``attiny1614``, ``attiny1616``, ``attiny1617``, ``attiny3214``, ``attiny3216``, ``attiny3217``, ``atmega808``, ``atmega809``, ``atmega1608``, ``atmega1609``, ``atmega3208``, ``atmega3209``, ``atmega4808``, ``atmega4809``, ``avr32da28``, ``avr32da32``, ``avr32da48``, ``avr32db28``, ``avr32db32``, ``avr32db48``.

``avrxmega4``
  "XMEGA" devices with more than 64 |nbsp| KiB and up to 128 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atxmega64a3``, ``atxmega64a3u``, ``atxmega64a4u``, ``atxmega64b1``, ``atxmega64b3``, ``atxmega64c3``, ``atxmega64d3``, ``atxmega64d4``, ``avr128da28``, ``avr128da32``, ``avr128da48``, ``avr128da64``, ``avr128db28``, ``avr128db32``, ``avr128db48``, ``avr128db64``.

``avrxmega5``
  "XMEGA" devices with more than 64 |nbsp| KiB and up to 128 |nbsp| KiB of program memory and more than 64 |nbsp| KiB of RAM.

  :samp:`{mcu}=` ``atxmega64a1``, ``atxmega64a1u``.

``avrxmega6``
  "XMEGA" devices with more than 128 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``atxmega128a3``, ``atxmega128a3u``, ``atxmega128b1``, ``atxmega128b3``, ``atxmega128c3``, ``atxmega128d3``, ``atxmega128d4``, ``atxmega192a3``, ``atxmega192a3u``, ``atxmega192c3``, ``atxmega192d3``, ``atxmega256a3``, ``atxmega256a3b``, ``atxmega256a3bu``, ``atxmega256a3u``, ``atxmega256c3``, ``atxmega256d3``, ``atxmega384c3``, ``atxmega384d3``.

``avrxmega7``
  "XMEGA" devices with more than 128 |nbsp| KiB of program memory and more than 64 |nbsp| KiB of RAM.

  :samp:`{mcu}=` ``atxmega128a1``, ``atxmega128a1u``, ``atxmega128a4u``.

``avrtiny``
  "TINY" Tiny core devices with 512 |nbsp| B up to 4 |nbsp| KiB of program memory.

  :samp:`{mcu}=` ``attiny4``, ``attiny5``, ``attiny9``, ``attiny10``, ``attiny20``, ``attiny40``.

``avr1``
  This ISA is implemented by the minimal AVR core and supported for assembler only.

  :samp:`{mcu}=` ``attiny11``, ``attiny12``, ``attiny15``, ``attiny28``, ``at90s1200``.

