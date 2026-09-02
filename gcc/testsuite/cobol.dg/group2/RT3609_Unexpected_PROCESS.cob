      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-O0 -Wno-ibm-cdf -I copybooks -Wno-recording-mode -ffixed-form -dialect ibm -fno-static-call -fexec-charset=CP037 -fexec-national-charset=CP037" }

00001  PROCESS FLAG(I,W) FDUMP
00002  IDENTIFICATION DIVISION.
00003     PROGRAM-ID. prog.
00004  PROCEDURE DIVISION.
00005    DISPLAY "OK".

