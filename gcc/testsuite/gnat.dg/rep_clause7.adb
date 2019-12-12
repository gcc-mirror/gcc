procedure Rep_Clause7 is

   subtype Msg is String (1 .. 3);

   type Root is tagged record
     B : Boolean;
     M : Msg;
   end record;
   for Root use record
     B at 0 range 64 .. 64;
     M at 0 range 65 .. 88;
   end record;

   type Ext is new Root with null record;

   procedure Inner (T : Msg) is
   begin
      null;
   end;

   pragma Warnings (Off);
   T1 : Root;
   T2 : Ext;
   pragma Warnings (On);

begin
   Inner (T1.M);
   Inner (T2.M);
end;
