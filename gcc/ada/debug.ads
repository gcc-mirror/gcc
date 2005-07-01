------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                D E B U G                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package Debug is
pragma Preelaborate (Debug);

--  This package contains global flags used to control the inclusion
--  of debugging code in various phases of the compiler. Some of these
--  flags are also used by the binder and gnatmake.

   -------------------------
   -- Dynamic Debug Flags --
   -------------------------

   --  Sixty two flags that can be used to active various specialized
   --  debugging output information. The flags are preset to False, which
   --  corresponds to the given output being suppressed. The individual
   --  flags can be turned on using the undocumented switch dxxx where
   --  xxx is a string of letters for flags to be turned on. Documentation
   --  on the current usage of these flags is contained in the body of Debug
   --  rather than the spec, so that we don't have to recompile the world
   --  when a new debug flag is added

   Debug_Flag_A : Boolean := False;
   Debug_Flag_B : Boolean := False;
   Debug_Flag_C : Boolean := False;
   Debug_Flag_D : Boolean := False;
   Debug_Flag_E : Boolean := False;
   Debug_Flag_F : Boolean := False;
   Debug_Flag_G : Boolean := False;
   Debug_Flag_H : Boolean := False;
   Debug_Flag_I : Boolean := False;
   Debug_Flag_J : Boolean := False;
   Debug_Flag_K : Boolean := False;
   Debug_Flag_L : Boolean := False;
   Debug_Flag_M : Boolean := False;
   Debug_Flag_N : Boolean := False;
   Debug_Flag_O : Boolean := False;
   Debug_Flag_P : Boolean := False;
   Debug_Flag_Q : Boolean := False;
   Debug_Flag_R : Boolean := False;
   Debug_Flag_S : Boolean := False;
   Debug_Flag_T : Boolean := False;
   Debug_Flag_U : Boolean := False;
   Debug_Flag_V : Boolean := False;
   Debug_Flag_W : Boolean := False;
   Debug_Flag_X : Boolean := False;
   Debug_Flag_Y : Boolean := False;
   Debug_Flag_Z : Boolean := False;

   Debug_Flag_AA : Boolean := False;
   Debug_Flag_BB : Boolean := False;
   Debug_Flag_CC : Boolean := False;
   Debug_Flag_DD : Boolean := False;
   Debug_Flag_EE : Boolean := False;
   Debug_Flag_FF : Boolean := False;
   Debug_Flag_GG : Boolean := False;
   Debug_Flag_HH : Boolean := False;
   Debug_Flag_II : Boolean := False;
   Debug_Flag_JJ : Boolean := False;
   Debug_Flag_KK : Boolean := False;
   Debug_Flag_LL : Boolean := False;
   Debug_Flag_MM : Boolean := False;
   Debug_Flag_NN : Boolean := False;
   Debug_Flag_OO : Boolean := False;
   Debug_Flag_PP : Boolean := False;
   Debug_Flag_QQ : Boolean := False;
   Debug_Flag_RR : Boolean := False;
   Debug_Flag_SS : Boolean := False;
   Debug_Flag_TT : Boolean := False;
   Debug_Flag_UU : Boolean := False;
   Debug_Flag_VV : Boolean := False;
   Debug_Flag_WW : Boolean := False;
   Debug_Flag_XX : Boolean := False;
   Debug_Flag_YY : Boolean := False;
   Debug_Flag_ZZ : Boolean := False;

   Debug_Flag_1 : Boolean := False;
   Debug_Flag_2 : Boolean := False;
   Debug_Flag_3 : Boolean := False;
   Debug_Flag_4 : Boolean := False;
   Debug_Flag_5 : Boolean := False;
   Debug_Flag_6 : Boolean := False;
   Debug_Flag_7 : Boolean := False;
   Debug_Flag_8 : Boolean := False;
   Debug_Flag_9 : Boolean := False;

   Debug_Flag_Dot_A : Boolean := False;
   Debug_Flag_Dot_B : Boolean := False;
   Debug_Flag_Dot_C : Boolean := False;
   Debug_Flag_Dot_D : Boolean := False;
   Debug_Flag_Dot_E : Boolean := False;
   Debug_Flag_Dot_F : Boolean := False;
   Debug_Flag_Dot_G : Boolean := False;
   Debug_Flag_Dot_H : Boolean := False;
   Debug_Flag_Dot_I : Boolean := False;
   Debug_Flag_Dot_J : Boolean := False;
   Debug_Flag_Dot_K : Boolean := False;
   Debug_Flag_Dot_L : Boolean := False;
   Debug_Flag_Dot_M : Boolean := False;
   Debug_Flag_Dot_N : Boolean := False;
   Debug_Flag_Dot_O : Boolean := False;
   Debug_Flag_Dot_P : Boolean := False;
   Debug_Flag_Dot_Q : Boolean := False;
   Debug_Flag_Dot_R : Boolean := False;
   Debug_Flag_Dot_S : Boolean := False;
   Debug_Flag_Dot_T : Boolean := False;
   Debug_Flag_Dot_U : Boolean := False;
   Debug_Flag_Dot_V : Boolean := False;
   Debug_Flag_Dot_W : Boolean := False;
   Debug_Flag_Dot_X : Boolean := False;
   Debug_Flag_Dot_Y : Boolean := False;
   Debug_Flag_Dot_Z : Boolean := False;

   Debug_Flag_Dot_AA : Boolean := False;
   Debug_Flag_Dot_BB : Boolean := False;
   Debug_Flag_Dot_CC : Boolean := False;
   Debug_Flag_Dot_DD : Boolean := False;
   Debug_Flag_Dot_EE : Boolean := False;
   Debug_Flag_Dot_FF : Boolean := False;
   Debug_Flag_Dot_GG : Boolean := False;
   Debug_Flag_Dot_HH : Boolean := False;
   Debug_Flag_Dot_II : Boolean := False;
   Debug_Flag_Dot_JJ : Boolean := False;
   Debug_Flag_Dot_KK : Boolean := False;
   Debug_Flag_Dot_LL : Boolean := False;
   Debug_Flag_Dot_MM : Boolean := False;
   Debug_Flag_Dot_NN : Boolean := False;
   Debug_Flag_Dot_OO : Boolean := False;
   Debug_Flag_Dot_PP : Boolean := False;
   Debug_Flag_Dot_QQ : Boolean := False;
   Debug_Flag_Dot_RR : Boolean := False;
   Debug_Flag_Dot_SS : Boolean := False;
   Debug_Flag_Dot_TT : Boolean := False;
   Debug_Flag_Dot_UU : Boolean := False;
   Debug_Flag_Dot_VV : Boolean := False;
   Debug_Flag_Dot_WW : Boolean := False;
   Debug_Flag_Dot_XX : Boolean := False;
   Debug_Flag_Dot_YY : Boolean := False;
   Debug_Flag_Dot_ZZ : Boolean := False;

   Debug_Flag_Dot_1 : Boolean := False;
   Debug_Flag_Dot_2 : Boolean := False;
   Debug_Flag_Dot_3 : Boolean := False;
   Debug_Flag_Dot_4 : Boolean := False;
   Debug_Flag_Dot_5 : Boolean := False;
   Debug_Flag_Dot_6 : Boolean := False;
   Debug_Flag_Dot_7 : Boolean := False;
   Debug_Flag_Dot_8 : Boolean := False;
   Debug_Flag_Dot_9 : Boolean := False;

   function Get_Debug_Flag_K return Boolean;
   --  This function is called from C code to get the setting of the K flag
   --  (it does not work to try to access a constant object directly).

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True);
   --  Where C is 0-9, A-Z, or a-z, sets the corresponding debug flag to
   --  the given value. In the checks off version of debug, the call to
   --  Set_Debug_Flag is always a null operation.

   procedure Set_Dotted_Debug_Flag (C : Character; Val : Boolean := True);
   --  Where C is 0-9, A-Z, or a-z, sets the corresponding dotted debug
   --  flag (e.g. call with C = 'a' for the .a flag).

end Debug;
