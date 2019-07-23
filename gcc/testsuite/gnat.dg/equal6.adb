--  { dg-do run }
with Text_IO;
with Equal6_Types; use Equal6_Types;

procedure Equal6 is
   Packets_In  : To_Evc_Optional_Packet_List_T;
   Packets_Out : To_Evc_Optional_Packet_List_T;
begin
   Packets_In.list (1) :=
     (Data_Used_Outside_Ertms_System =>
        (Mail_Box    =>
           (Receiver => 31,
            Data     => (Length => 12, Message => (0, others => 0)))));

   Packets_Out.list (1) :=
     (Data_Used_Outside_Ertms_System =>
        (Mail_Box    =>
           (Receiver => 31,
            Data     => (Length => 12, Message => (0, others => 1)))));

   if not (Packets_In = Packets_Out) then
      raise Program_Error;
   end if;

   if not (Equal1_Called and then Equal2_Called) then
      raise Program_Error;
   end if;

end Equal6;
