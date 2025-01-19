------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 I N T E R F A C E S . V X W O R K S . I O                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--           Copyright (C) 2002-2025, Free Software Foundation, Inc.        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a binding to the functions fileno and ioctl
--  in VxWorks, providing a set of definitions of ioctl function codes
--  and options for the use of these functions.

--  A particular use of this interface is to enable use of Get_Immediate
--  in Ada.Text_IO. There is no way in VxWorks to provide the desired
--  functionality of Get_Immediate (no buffering and no waiting for a
--  line return) without flushing the buffer, which violates the Ada
--  semantic requirements for Ada.Text_IO.

with Interfaces.C_Streams;

package Interfaces.VxWorks.IO is

   -------------------------
   -- The ioctl Interface --
   --------------------------

   type FUNCODE is new int;
   --  Type of the function codes in ioctl

   type IOOPT is mod 2 ** int'Size;
   --  Type of the option codes in ioctl

   --  ioctl function codes (for more information see ioLib.h)
   --  These values could be generated automatically in System.OS_Constants???

   FIONREAD       : constant FUNCODE := 1;
   FIOFLUSH       : constant FUNCODE := 2;
   FIOOPTIONS     : constant FUNCODE := 3;
   FIOBAUDRATE    : constant FUNCODE := 4;
   FIODISKFORMAT  : constant FUNCODE := 5;
   FIODISKINIT    : constant FUNCODE := 6;
   FIOSEEK        : constant FUNCODE := 7;
   FIOWHERE       : constant FUNCODE := 8;
   FIODIRENTRY    : constant FUNCODE := 9;
   FIORENAME      : constant FUNCODE := 10;
   FIOREADYCHANGE : constant FUNCODE := 11;
   FIONWRITE      : constant FUNCODE := 12;
   FIODISKCHANGE  : constant FUNCODE := 13;
   FIOCANCEL      : constant FUNCODE := 14;
   FIOSQUEEZE     : constant FUNCODE := 15;
   FIONBIO        : constant FUNCODE := 16;
   FIONMSGS       : constant FUNCODE := 17;
   FIOGETNAME     : constant FUNCODE := 18;
   FIOGETOPTIONS  : constant FUNCODE := 19;
   FIOSETOPTIONS  : constant FUNCODE := FIOOPTIONS;
   FIOISATTY      : constant FUNCODE := 20;
   FIOSYNC        : constant FUNCODE := 21;
   FIOPROTOHOOK   : constant FUNCODE := 22;
   FIOPROTOARG    : constant FUNCODE := 23;
   FIORBUFSET     : constant FUNCODE := 24;
   FIOWBUFSET     : constant FUNCODE := 25;
   FIORFLUSH      : constant FUNCODE := 26;
   FIOWFLUSH      : constant FUNCODE := 27;
   FIOSELECT      : constant FUNCODE := 28;
   FIOUNSELECT    : constant FUNCODE := 29;
   FIONFREE       : constant FUNCODE := 30;
   FIOMKDIR       : constant FUNCODE := 31;
   FIORMDIR       : constant FUNCODE := 32;
   FIOLABELGET    : constant FUNCODE := 33;
   FIOLABELSET    : constant FUNCODE := 34;
   FIOATTRIBSE    : constant FUNCODE := 35;
   FIOCONTIG      : constant FUNCODE := 36;
   FIOREADDIR     : constant FUNCODE := 37;
   FIOFSTATGET    : constant FUNCODE := 38;
   FIOUNMOUNT     : constant FUNCODE := 39;
   FIOSCSICOMMAND : constant FUNCODE := 40;
   FIONCONTIG     : constant FUNCODE := 41;
   FIOTRUNC       : constant FUNCODE := 42;
   FIOGETFL       : constant FUNCODE := 43;
   FIOTIMESET     : constant FUNCODE := 44;
   FIOINODETONAM  : constant FUNCODE := 45;
   FIOFSTATFSGE   : constant FUNCODE := 46;

   --  ioctl option values

   OPT_ECHO     : constant IOOPT := 16#0001#;
   OPT_CRMOD    : constant IOOPT := 16#0002#;
   OPT_TANDEM   : constant IOOPT := 16#0004#;
   OPT_7_BIT    : constant IOOPT := 16#0008#;
   OPT_MON_TRAP : constant IOOPT := 16#0010#;
   OPT_ABORT    : constant IOOPT := 16#0020#;
   OPT_LINE     : constant IOOPT := 16#0040#;
   OPT_RAW      : constant IOOPT := 16#0000#;
   OPT_TERMINAL : constant IOOPT := OPT_ECHO     or
                                    OPT_CRMOD    or
                                    OPT_TANDEM   or
                                    OPT_MON_TRAP or
                                    OPT_7_BIT    or
                                    OPT_ABORT    or
                                    OPT_LINE;

   function fileno (Fp : Interfaces.C_Streams.FILEs) return int;
   pragma Import (C, fileno, "fileno");
   --  Binding to the C routine fileno

   function ioctl (Fd : int; Function_Code : FUNCODE; Arg : IOOPT) return int;
   pragma Import (C, ioctl, "ioctl");
   --  Binding to the C routine ioctl
   --
   --  Note: we are taking advantage of the fact that on currently supported
   --  VxWorks targets, it is fine to directly bind to a variadic C function.

   ------------------------------
   -- Control of Get_Immediate --
   ------------------------------

   --  The procedures in this section make use of the interface to ioctl
   --  and fileno to provide a mechanism for enabling unbuffered behavior
   --  for Get_Immediate in VxWorks.

   --  The situation is that the RM requires that the use of Get_Immediate
   --  be identical to Get except that it is desirable (not required) that
   --  there be no buffering or line editing.

   --  Unfortunately, in VxWorks, the only way to enable this desired
   --  unbuffered behavior involves changing into raw mode. But this
   --  transition into raw mode flushes the input buffer, a behavior
   --  not permitted by the RM semantics for Get_Immediate.

   --  Given that Get_Immediate cannot be accurately implemented in
   --  raw mode, it seems best not to enable it by default, and instead
   --  to require specific programmer action, with the programmer being
   --  aware that input may be lost.

   --  The following is an example of the use of the two procedures
   --  in this section (Enable_Get_Immediate and Disable_Get_Immediate)

   --  with Ada.Text_IO; use Ada.Text_IO;
   --  with Ada.Text_IO.C_Streams; use Ada.Text_IO.C_Streams;
   --  with Interfaces.VxWorks.IO; use Interfaces.VxWorks.IO;

   --  procedure Example_IO is
   --     Input     : Character;
   --     Available : Boolean;
   --     Success   : Boolean;

   --  begin
   --     Enable_Get_Immediate (C_Stream (Current_Input), Success);

   --     if Success = False then
   --        raise Device_Error;
   --     end if;

   --     --  Example with the first type of Get_Immediate
   --     --  Waits for an entry on the input. Immediately returns
   --     --  after having received an character on the input

   --     Put ("Input -> ");
   --     Get_Immediate (Input);
   --     New_Line;
   --     Put_Line ("Character read: " & Input);

   --     --  Example with the second type of Get_Immediate
   --     --  This is equivalent to a non blocking read

   --     for J in 1 .. 10 loop
   --        Put ("Input -> ");
   --        Get_Immediate (Input, Available);
   --        New_Line;

   --        if Available = True then
   --           Put_Line ("Character read: " & Input);
   --        end if;

   --        delay 1.0;
   --     end loop;

   --     Disable_Get_Immediate (C_Stream (Current_Input), Success);

   --     if Success = False then
   --        raise Device_Error;
   --     end if;

   --  exception
   --     when Device_Error =>
   --        Put_Line ("Device Error. Check your configuration");
   --  end Example_IO;

   procedure Enable_Get_Immediate
     (File    : Interfaces.C_Streams.FILEs;
      Success : out Boolean);
   --  On VxWorks, a call to this procedure is required before subsequent calls
   --  to Get_Immediate have the desired effect of not waiting for a line
   --  return. The reason that this call is not automatic on this target is
   --  that the call flushes the input buffer, discarding any previous input.
   --  Note: Following a call to Enable_Get_Immediate, the only permitted
   --  operations on the relevant file are Get_Immediate operations. Any
   --  other operations have undefined behavior.

   procedure Disable_Get_Immediate
     (File    : Interfaces.C_Streams.FILEs;
      Success : out Boolean);
   --  This procedure resets File to standard mode, and permits subsequent
   --  use of the full range of Ada.Text_IO functions

end Interfaces.VxWorks.IO;
