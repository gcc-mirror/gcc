package Equal6_Types is
   type Bit_T is range 0 .. 1;

   type Bits_T is array (Positive range <>) of Bit_T;

   type Nid_Xuser_T is range 0 .. 511;

   Dispatch_P44_To_Ntc_C : constant Nid_Xuser_T := 102;

   type Mail_Box_Data_T is record
      Length  : Natural;
      Message : Bits_T (1 .. 200);
   end record;
   function "=" (L, R : in Mail_Box_Data_T) return Boolean;
   Equal1_Called : Boolean := False;

   type Mail_Box_T (Receiver : Nid_Xuser_T := Nid_Xuser_T'First) is record
      Data : Mail_Box_Data_T;
      case Receiver is
         when Dispatch_P44_To_Ntc_C =>
            Stm_Id : Positive;
         when others =>
            null;
      end case;
   end record;

   type Data_Used_Outside_Ertms_System_T is record
      Mail_Box    : Mail_Box_T;
   end record;

   type To_Evc_Optional_Packet_T
   is record
            Data_Used_Outside_Ertms_System : Data_Used_Outside_Ertms_System_T;
   end record;

   type To_Evc_Optional_Packet_List_Length_T is range 0 .. 50;
   type To_Evc_Optional_Packet_Map_T is
     array
       (To_Evc_Optional_Packet_List_Length_T range <>)
            of To_Evc_Optional_Packet_T;

   type To_Evc_Optional_Packet_List_T is record
      List : To_Evc_Optional_Packet_Map_T
        (1 .. To_Evc_Optional_Packet_List_Length_T'Last);
   end record;
   function "=" (L, R : in To_Evc_Optional_Packet_List_T) return Boolean;
   Equal2_Called : Boolean := False;

end Equal6_Types;
