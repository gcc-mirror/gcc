-- FD72A00.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- FOUNDATION DESCRIPTION:
--      This foundation provides a basis for testing package
--      System.Address_To_Access_Conversions
--
-- TEST FILES:
--      The following files comprise this foundation:
--
--         FD72A00.A
--
-- CHANGE HISTORY:
--      08 FEB 96   SAIC    Initial version
--
--!

with Impdef;
with System.Storage_Elements;
package FD72A00 is
  use System;

  subtype Number is System.Storage_Elements.Integer_Address;

  package Num_IO renames Impdef.Address_Value_IO;

    -- the following conversions To/From Hex are to prevent optimizers from
    -- optimizing out the otherwise senseless identity conversions, and
    -- given the unknown nature of the type Number, the Identity operations
    -- provided in Report will not suffice to this cause.

    function Address_To_Hex( Adder: System.Address ) return String;

    function Hex_To_Address( Hex: access String ) return System.Address;

end FD72A00;

package body FD72A00 is

    function Address_To_Hex( Adder: System.Address ) return String is
      S : String(1..64)
       := "uninitializedDEFuninitializedDEFuninitializedDEFuninitializedDEF";
      DeBlank : Positive := S'First;
    begin
      Num_IO.Put( S, Number( System.Storage_Elements.To_Integer( Adder ) ),
                  Base => 16 );
      while S(DeBlank) = ' ' loop
        DeBlank := DeBlank +1;
      end loop;
      return S(DeBlank..S'Last);
    end Address_To_Hex;

    function Hex_To_Address( Hex: access String ) return System.Address is
      The_Number : Number;
      Tail       : Natural;
    begin
      Num_IO.Get( Hex.all, The_Number, Tail );
      return System.Storage_Elements.To_Address(
                      System.Storage_Elements.Integer_Address( The_Number ) );
    end Hex_To_Address;

end FD72A00;
