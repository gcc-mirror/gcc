------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . W I D E _ T E X T _ IO . C O M P L E X _ I O           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Types;

generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (<>);

package Ada.Wide_Text_IO.Complex_IO is

   Default_Fore : Field := 2;
   Default_Aft  : Field := Complex_Types.Real'Digits - 1;
   Default_Exp  : Field := 3;

   procedure Get
     (File  : File_Type;
      Item  : out Complex_Types.Complex;
      Width : Field := 0);

   procedure Get
     (Item  : out Complex_Types.Complex;
      Width : Field := 0);

   procedure Put
     (File : File_Type;
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Put
     (Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Get
     (From : Wide_String;
      Item : out Complex_Types.Complex;
      Last : out Positive);

   procedure Put
     (To   : out Wide_String;
      Item : Complex_Types.Complex;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

end Ada.Wide_Text_IO.Complex_IO;
