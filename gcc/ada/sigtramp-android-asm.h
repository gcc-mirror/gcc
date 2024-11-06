/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                      S I G T R A M P - T A R G E T                       *
 *                                                                          *
 *                     Asm Implementation Include File                      *
 *                                                                          *
 *           Copyright (C) 2024, Free Software Foundation, Inc.             *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * In particular,  you can freely  distribute your programs  built with the *
 * GNAT Pro compiler, including any required library run-time units,  using *
 * any licensing terms  of your choosing.  See the AdaCore Software License *
 * for full details.                                                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*****************************************************************
 * CPU specific parts of the __gnat_sigtramp service for Android *
 *****************************************************************/

/* The intended use mode of this header is to provide macros
   and a prologue to the generation of an asm function, as in

     #include <this-header>
     asm (SIGTRAMP_START(<symbol-name>));
     asm (SIGTRAMP_BODY);
     asm (SIGTRAMP_END(<symbol-name>));

   and nothing else after.  */

/* asm string construction helpers.  */

#define STR(TEXT) #TEXT
/* stringify expanded TEXT, surrounding it with double quotes.  */

#define S(E) STR(E)
/* stringify E, which will resolve as text but may contain macros
   still to be expanded.  */

/* asm (TEXT) outputs <tab>TEXT. These facilitate the output of
   multiline contents:  */
#define TAB(S) "\t" S
#define CR(S)  S "\n"

#undef TCR
#define TCR(S) TAB(CR(S))

#if defined(__arm__)

/* Trampoline body block
   ---------------------  */

#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and also save r2 which is the argument register") \
TCR("# containing the sigcontext, so that we can restore it during") \
TCR("# unwinding and thereby load the rest of the desired context.") \
TCR("stmfd	sp!, {r2, r3, lr}") \
TCR("# The unwinder undo's these operations in reverse order so starting") \
TCR("# from bottom, restore r2 from the current vsp location, move r2 into") \
TCR("# the vsp, add 12 bytes to get the start of the register save area") \
TCR("# then restore the 15 general purpose registers of the frame which") \
TCR("# raised the signal.") \
TCR(".save {r0-r15}") \
TCR(".pad #12") \
TCR(".movsp r2") \
TCR(".save {r2}") \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received in r0, r1 and r2.") \
TCR("blx	r3") \
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("# (should never get here!).") \
TCR("ldmfd	sp, {r2, r3, pc}")

/* Symbol definition block
   -----------------------  */

#define SIGTRAMP_START(SYM) \
CR("# " S(SYM) " unwind trampoline") \
TCR(".type " S(SYM) ", %function") \
CR("") \
CR(S(SYM) ":") \
TCR(".fnstart")

/* Symbol termination block
   ------------------------  */

#define SIGTRAMP_END(SYM) \
CR(".fnend") \
TCR(".size " S(SYM) ", .-" S(SYM))

#endif

/* Text section start.  The compiler isn't aware of that switch.  */

asm (".text\n"
     TCR(".align 2"));
