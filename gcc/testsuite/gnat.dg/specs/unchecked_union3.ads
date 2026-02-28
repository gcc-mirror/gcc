-- { dg-do compile }

with Interfaces;

package Unchecked_Union3 is

   type T_CAN_ID is mod 2**29 with
      Default_Value => 0,
      Size          => 29;

   type T_Node_ID is mod 2**7 with
      Size          => 7,
      Default_Value => 0;

   type T_Message_Type_ID is new Interfaces.Unsigned_16;

   type T_Service_Type_ID is new Interfaces.Unsigned_8;

   type T_Priority is mod 2**5 with
      Size          => 5,
      Default_Value => 0;

   type T_Format_Selector is
     (CAN_Fields,
      DroneCAN_Fields);

   type T_Frame_Selector is
     (Message_Frame,
      Anonymous_Frame,
      Service_Frame);

   type Unsigned_2 is mod 2**2 with
      Size          => 2,
      Default_Value => 0;

   type Unsigned_14 is mod 2**14 with
      Size          => 14,
      Default_Value => 0;

   type T_Header (Format_Selector : T_Format_Selector := DroneCAN_Fields;
      Frame_Selector              : T_Frame_Selector  := Message_Frame) is record
      case Format_Selector is
         when CAN_Fields =>
            CAN_ID : T_CAN_ID;

         when DroneCAN_Fields =>
            Source_Node_ID      : T_Node_ID;
            Service_Not_Message : Boolean := False;
            Priority            : T_Priority;

            case Frame_Selector is
               when Message_Frame =>
                  Message_Type_ID : T_Message_Type_ID;
               when Anonymous_Frame =>
                  Message_Type_ID_Lower_Bits : Unsigned_2;
                  Discriminator              : Unsigned_14;
               when Service_Frame =>
                  Destination_Node_ID  : T_Node_ID;
                  Request_Not_Response : Boolean := False;
                  Service_Type_ID      : T_Service_Type_ID;
            end case;
      end case;
   end record with
      Unchecked_Union,
      Size => 29;

   for T_Header use record
      CAN_ID                     at 0 range  0 .. 28;
      Source_Node_ID             at 0 range  0 ..  6;
      Service_Not_Message        at 0 range  7 ..  7;
      Priority                   at 0 range 24 .. 28;
      Message_Type_ID            at 0 range  8 .. 23;
      Message_Type_ID_Lower_Bits at 0 range  8 ..  9;
      Discriminator              at 0 range 10 .. 23;
      Destination_Node_ID        at 0 range  8 .. 14;
      Request_Not_Response       at 0 range 15 .. 15;
      Service_Type_ID            at 0 range 16 .. 23;
   end record;

end Unchecked_Union3;
