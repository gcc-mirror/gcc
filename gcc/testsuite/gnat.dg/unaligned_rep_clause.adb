-- { dg-do compile }

procedure Unaligned_Rep_Clause is

   type One_Bit_Record is
      record
         B : Boolean;
      end record;
   Pragma Pack(One_Bit_Record);

   subtype Version_Number_Type is String (1 .. 3);

   type Inter is
      record
         Version : Version_Number_Type;
      end record;

   type Msg_Type is
      record
         Status  : One_Bit_Record;
         Version : Inter;
      end record;

   for Msg_Type use
      record
         Status  at 0 range 0 .. 0;
         Version at 0 range 1 .. 24;
      end record;
   for Msg_Type'Size use 25;

   Data : Msg_Type;
   Pragma Warnings (Off, Data);
   Version : Inter;

begin
   Version := Data.Version;
end;
